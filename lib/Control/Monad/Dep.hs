{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--    This package provides 'DepT', a monad transformer similar to 'ReaderT'.
--
--    The difference is that the environment of 'DepT' must be parameterized by @DepT@'s own monad stack.
module Control.Monad.Dep
  ( DepT (DepT),
    runDepT,
    toReaderT,
  )
where

import Control.Applicative
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Class
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
  ( (forall x. DepT env m x -> DepT env' m x) ->
    env' (DepT env' m) ->
    env' (DepT env m)
  ) ->
  (forall t. env' t -> env t) ->
  DepT env m a ->
  DepT env' m a
withDepT transMonad enhanceEnv (DepT rm) = 
    DepT (withReaderT (enhanceEnv . transMonad (withDepT transMonad enhanceEnv)) rm)
