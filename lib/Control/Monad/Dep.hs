{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Dep where

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

type DepT ::
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type ->
  Type
newtype DepT env m r = DepT {_runDepT :: ReaderT (env (DepT env m)) m r}
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

runDepT :: DepT env m r -> env (DepT env m) -> m r
runDepT = runReaderT . _runDepT
