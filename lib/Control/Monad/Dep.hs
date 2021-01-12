{-# LANGUAGE StandaloneKindSignatures, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Control.Monad.Dep where

import Data.Kind (Type)
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.IO.Unlift

type DepT :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type -> Type
newtype DepT env m r = DepT { _runDepT :: ReaderT (env (DepT env m)) m r } 
    deriving (Functor,Applicative,Monad,MonadIO,MonadUnliftIO,MonadReader (env (DepT env m)))

instance MonadTrans (DepT env) where
    lift = DepT . lift

runDepT :: DepT env m r -> env (DepT env m) -> m r
runDepT  = runReaderT . _runDepT

