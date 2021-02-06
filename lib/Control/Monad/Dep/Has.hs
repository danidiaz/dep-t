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

module Control.Monad.Dep.Has (Has (..), DepDefaults (..)) where

import Data.Kind
import GHC.Records
import GHC.TypeLits

type Has :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type -> Constraint
class Has k d e | e -> d where
  dep :: e -> k d
  default dep :: (DepDefaults k, HasField (DefaultFieldName k) e (k d)) => e -> k d
  dep = getField @(DefaultFieldName k)

type DepDefaults :: k -> Constraint
class DepDefaults k where
  -- The Char kind would be useful here, to lowercase the first letter of the
  -- k type and use it as the default preferred field name.
  type DefaultFieldName k :: Symbol

