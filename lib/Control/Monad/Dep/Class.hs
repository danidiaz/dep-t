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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Dep.Class (
        -- * Reader-like monads carrying dependencies in their environment
        MonadDep,
        -- * Lifting effects from dependencies 
        LiftDep (..),
    ) where

import Control.Monad.Reader
import Data.Kind

type LiftDep :: (Type -> Type) -> (Type -> Type) -> Constraint
class LiftDep d m where
    liftD :: d x -> m x 

instance (Monad m, MonadTrans t) => LiftDep m (t m) where
    liftD = lift

type MonadDep :: [Type -> (Type -> Type) -> Constraint] -> Type -> (Type -> Type) -> (Type -> Type) -> Constraint
type family MonadDep capabilities e d m where
    MonadDep '[] e d m = (MonadReader e m, LiftDep d m)
    MonadDep (capability ': capabilities) e d m  = (capability e d, MonadDep capabilities e d m)

