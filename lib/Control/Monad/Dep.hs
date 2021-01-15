{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}

--    This package provides 'DepT', a monad transformer similar to 'ReaderT'.
--
--    The difference is that the environment of 'DepT' must be parameterized by
--    @DepT@'s own monad stack.
--
--    There's a function 'withDepT' which is analogous to 'withReaderT'. 
--
--    There's no analogue of 'mapReaderT' however. This means you can't tweak
--    the monad below the 'DepT' with a natural transformation.
module Control.Monad.Dep
  ( DepT (DepT),
    runDepT,
    toReaderT,
    withDepT,
    zoomEnv,
  )
where

import Control.Applicative
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Writer.Class
import Control.Monad.Zip
import Data.Kind
import Rank2 qualified
import Rank2.TH qualified
-- |
-- |
--    A monad transformer, which adds a read-only environment to the given monad.
--    The environment type must be parameterized with the transformer's stack.
--
--    The 'return' function ignores the environment, while @>>=@ passes the
--    inherited environment to both subcomputations.
type DepT ::
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type ->
  Type
newtype DepT env m r = DepT {toReaderT :: ReaderT (env (DepT env m)) m r}
  deriving
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadFix,
      MonadFail,
      MonadZip,
      MonadPlus,
      MonadCont,
      MonadIO,
      MonadUnliftIO,
      MonadReader (env (DepT env m))
    )

instance MonadTrans (DepT env) where
  lift = DepT . lift

deriving instance MonadState s m => MonadState s (DepT env m)

deriving instance MonadWriter w m => MonadWriter w (DepT env m)

deriving instance MonadError e m => MonadError e (DepT env m)

-- |
--    Runs a 'DepT' action in an environment.
runDepT :: DepT env m r -> env (DepT env m) -> m r
runDepT = runReaderT . toReaderT

{-
   I'm overcomplicating things, aren't I?
-}
withDepT ::
  forall small big m a.
  Monad m =>
  ( forall p q.
    (forall x. p x -> q x) ->
    small p ->
    small q
  ) ->
  (forall t. big t -> small t) ->
  DepT small m a ->
  DepT big m a
withDepT mapEnv inner (DepT (ReaderT f)) =
  DepT
    ( ReaderT
        ( \big ->
            let small :: small (DepT small m)
                -- we have a big environment at hand, so let's extract the
                -- small environment, transform every function in the small
                -- environment by supplying the big environment and, as a
                -- finishing touch, lift from the base monad m so that it
                -- matches the monad expected by f.
                small = mapEnv (lift . flip runDepT big) (inner big)
             in f small
        )
    )

{-
   Makes the functions inside a small environment require a bigger environment. 

   This can be useful if we are encasing the small environment as a field of
   the big environment, ir order to make the types match.
 -}
zoomEnv ::
  forall small big m a.
  Monad m =>
  ( forall p q.
    (forall x. p x -> q x) ->
    small p ->
    small q
  ) ->
  (forall t. big t -> small t) ->
  small (DepT small m) ->
  small (DepT big m)
zoomEnv mapEnv inner = mapEnv (withDepT mapEnv inner)

--
-- BUUUUG

-- The environment doesn't know about any concrete monad
type Env :: (Type -> Type) -> Type
data Env m = Env
  { logger :: String -> m (),
    logic :: Int -> m Int
  }
$(Rank2.TH.deriveFunctor ''Env)

-- Has-style typeclasses can be provided to avoid depending on concrete
-- environments.
-- Note that the environment determines the monad.
type HasLogger :: Type -> (Type -> Type) -> Constraint
class HasLogger r m | r -> m where
  getLogger :: r -> String -> m ()

-- If our environment is parmeterized by the monad m, then logging is done in
-- m.
instance HasLogger (Env m) m where
  getLogger = logger

-- These two functions don't know the concrete envionment record.
--
-- This one because it only needs MonadIO.
_logger :: MonadIO m => String -> m ()
_logger msg = liftIO (putStrLn msg)

-- This one because it receives a getter for the logger
-- A HasX-style typeclass would have been an alternative. 
_logic :: MonadReader e m => (e -> String -> m ()) -> Int -> m Int
_logic getLogger x = do
  logger <- reader getLogger
  logger "I'm going to multiply a number by itself!"
  return $ x * x

-- This is the first time DepT is used in this module.
-- Note that it is only here where we settle for IO.
env :: Env (DepT Env IO)
env =
  Env
    { logger = _logger,
      logic = _logic logger
    }

-- We select "logic" as the entrypoint and run it.
result :: IO Int
result = runDepT (logic env 7) env

-- An attempt with ReaderT which doesn't work
-- env' =
--   Env
--     { logger = __logic logger
--     }
-- 
-- result' :: IO Int
-- result' = runReaderT (logic env' 7) env'


-- The environment doesn't know about any concrete monad
type BiggerEnv :: (Type -> Type) -> Type
data BiggerEnv m = BiggerEnv
  { inner :: Env m,
    extra :: Int -> m Int
  }

biggerEnv :: BiggerEnv (DepT BiggerEnv IO)
biggerEnv = BiggerEnv 
    {
        inner = (zoomEnv (Rank2.<$>) inner env),
        extra = pure 
    }

main :: IO ()
main = do
    r <- runDepT ((logic . inner $ biggerEnv) 7) biggerEnv
    print r
