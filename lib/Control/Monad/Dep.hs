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

-- |
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
import Data.Kind (Type)

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

