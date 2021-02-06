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

module Control.Monad.Dep.Has (Has (..), DepMarker (..)) where

import Data.Kind
import GHC.Records
import GHC.TypeLits

type Has :: k -> (Type -> Type) -> Type -> Constraint
class Has k d e | e -> d where
  type The k d e :: Type
  type The k d e = ExtractedDepType k d
  the :: forall k d e . e -> The k d e
  default the :: forall k d e. (DepMarker k d, HasField (PreferredFieldName k d) e (The k d e)) => e -> The k d e
  the = getField @(PreferredFieldName k d)

type DepMarker :: k -> (Type -> Type) -> Constraint
class DepMarker k d where
  -- The Char kind would be useful here, to lowercase the first letter of the
  -- k type and use it as the default preferred field name.
  type PreferredFieldName k d :: Symbol
  type ExtractedDepType k d :: Type
