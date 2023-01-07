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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeOperators #-}

-- | Inject values into accumulators.
module Dep.Injects (
    -- * General-purpose injector.
    Injects (..)
    ) where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Data.Coerce

-- | Mirror image of 'Dep.Has.Has'.
--
-- Can be useful with 'Dep.Constructor.AccumConstructor' to register particular
-- monoidal values into a \"wider"\ monoidal value which is accumulated accross
-- all components.
type Injects :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type -> Constraint
class Injects r_ (m :: Type -> Type) (accum :: Type) | accum -> m where
    -- | Given a value parameterized by the accumulator's effect monad @m@,
    -- produce an accumulator.
    inject :: r_ m -> env




