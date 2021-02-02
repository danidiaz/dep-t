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
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Dep.Class where

import Control.Monad.Reader
import Data.Kind

-- type MonadDep ::  Type -> (Type -> Type) -> (Type -> Type) -> Constraint
-- class MonadReader e m => MonadDep e d m where
--     liftD :: d x -> m x 
-- 
-- instance MonadDep e IO (ReaderT e IO) where
--     liftD = lift

type LiftDep :: (Type -> Type) -> (Type -> Type) -> Constraint
class LiftDep d m where
    liftD :: d x -> m x 

instance (Monad m, MonadTrans t) => LiftDep m (t m) where
    liftD = lift

