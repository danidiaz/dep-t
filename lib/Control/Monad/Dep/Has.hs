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

module Control.Monad.Dep.Has (
        -- A generic \"Has\" typeclass.
        Has (..), 
        -- Component defaults.
        Dep (..)
    ) where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Data.Coerce

-- | A generic \"Has\" class. When partially applied to a parametrizable
-- record-of-functions @r_@, produces a 2-place constraint that can be later
-- used with "Control.Monad.Dep.Class".
type Has :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type -> Constraint
class Has r_ d e | e -> d where
  -- |  Given an environment @e@, produce a record-of-functions parameterized by the environment's effect monad @d@.
  dep :: e -> r_ d
  default dep :: (Dep r_, HasField (DefaultFieldName r_) e u, Coercible u (r_ d)) => e -> r_ d
  dep e = coerce . getField @(DefaultFieldName r_) $ e

-- | Parametrizable records-of-functions can implement this instance to specify
-- the default field name 'Has' expects for the component in the environment
-- record.
--
-- This allows defining 'Has' instances with empty bodies, thanks to
-- @DefaultSignatures@.
type Dep :: ((Type -> Type) -> Type) -> Constraint
class Dep r_ where
  -- The Char kind would be useful here, to lowercase the first letter of the
  -- k type and use it as the default preferred field name.
  type DefaultFieldName r_ :: Symbol

