{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Kind

--
-- GENERAL CODE
type DepT ::
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type ->
  Type
newtype DepT env m r = DepT {toReaderT :: ReaderT (env (DepT env m)) m r}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO
    )

instance MonadTrans (DepT env) where
  lift = DepT . lift


-- |
--    Runs a 'DepT' action in an environment.
runDepT :: DepT env m r -> env (DepT env m) -> m r
runDepT = runReaderT . toReaderT

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
--

-- CONCRETE CODE THAT REPRODUCES THE BUG
--
type Env :: (Type -> Type) -> Type
data Env m = Env
  { logger :: String -> m (),
    logic :: Int -> m Int
  }

mapEnv :: (forall x. n x -> m x) -> Env n -> Env m
mapEnv f (Env {logger,logic}) =
    Env { logger = f . logger, logic = f . logic }

env :: Env (DepT Env IO)
env =
  Env
    { logger = \msg -> liftIO (putStrLn msg),
      logic = \x -> return $ x*x
    }

env' :: Env (DepT Env IO)
env' = zoomEnv mapEnv id env

main :: IO ()
main = do
  r <- runDepT (logic env' 7) env'
  print r
