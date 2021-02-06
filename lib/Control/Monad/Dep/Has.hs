{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Dep.Has where

import Data.Kind
import GHC.Records
import GHC.TypeLits

type Has :: k -> (Type -> Type) -> Type -> Constraint
class Has k d e | e -> d where
    type The k d e :: Type
    type The k d e = ExtractedDepType k d
    the :: e -> The k d e
    default the :: (DepMarker k d, HasField (PreferredFieldName k d) e (The k d e)) => e -> The k d e
    the = getField @(PreferredFieldName k d)

type DepMarker :: k -> (Type -> Type) -> Constraint
class DepMarker k d where
    -- The Char kind would be useful here, to lowercase the first letter of the 
    -- k type and use it as the default preferred field name.
    type PreferredFieldName k d :: Symbol
    type ExtractedDepType k d :: Type
