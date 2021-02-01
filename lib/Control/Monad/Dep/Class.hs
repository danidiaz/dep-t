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

type MonadDep :: (Type -> Type) -> Type -> (Type -> Type) -> Constraint
class MonadReader e m => MonadDep d e m where
    liftD :: d x -> m x 

instance MonadDep IO e (ReaderT e IO) where
    liftD = lift

