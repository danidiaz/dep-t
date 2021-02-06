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

module Control.Monad.Dep.Has (Has (..), DepMarker (..)) where

import Data.Kind
import GHC.Records
import GHC.TypeLits

type Has :: k -> (Type -> Type) -> Type -> Constraint
class Has k d e | e -> d where
  type The k :: (Type -> Type) -> Type
  type The k = ExtractedDepType k
  the :: e -> The k d
  default the :: (DepMarker k, HasField (PreferredFieldName k) e (The k d)) => e -> The k d
  the = getField @(PreferredFieldName k)

type DepMarker :: k -> Constraint
class DepMarker k where
  -- The Char kind would be useful here, to lowercase the first letter of the
  -- k type and use it as the default preferred field name.
  type PreferredFieldName k :: Symbol
  -- Only works for type constructors which receive a monad
  type ExtractedDepType k :: (Type -> Type) -> Type

