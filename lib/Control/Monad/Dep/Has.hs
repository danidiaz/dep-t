{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Dep.Has (Has (..), Dep (..)) where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Data.Coerce

type Has :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type -> Constraint
class Has k d e | e -> d where
  dep :: e -> k d
  default dep :: (Dep k, HasField (DefaultFieldName k) e u, Coercible u (k d)) => e -> k d
  dep e = coerce . getField @(DefaultFieldName k) $ e

type Dep :: ((Type -> Type) -> Type) -> Constraint
class Dep k where
  -- The Char kind would be useful here, to lowercase the first letter of the
  -- k type and use it as the default preferred field name.
  type DefaultFieldName k :: Symbol

