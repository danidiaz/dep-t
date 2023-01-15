{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Inject values into accumulators.
module Dep.Injects
  ( -- * General-purpose injector.
    Injects (..),
    InjectsAll
  )
where

import Data.Coerce
import Data.Kind
import GHC.Records
import GHC.TypeLits

-- | Mirror image of 'Dep.Has.Has'.
--
-- Can be useful with 'Dep.Constructor.AccumConstructor' to register particular
-- monoidal values into a \"wider"\ monoidal value which is accumulated accross
-- all components.
type Injects :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type -> Constraint
class Injects r_ (m :: Type -> Type) (accum :: Type) | accum -> m where
  -- | Given a value parameterized by the accumulator's effect monad @m@,
  -- produce an accumulator.
  inject :: r_ m -> accum

instance (Monoid b) => Injects r_ m (r_ m, b) where
  inject r = (r, mempty)

instance (Monoid a) => Injects r_ m (a, r_ m) where
  inject r = (mempty, r)

instance (Monoid b, Monoid c) => Injects r_ m (r_ m, b, c) where
  inject r = (r, mempty, mempty)

instance (Monoid a, Monoid c) => Injects r_ m (a, r_ m, c) where
  inject r = (mempty, r, mempty)

instance (Monoid a, Monoid b) => Injects r_ m (a, b, r_ m) where
  inject r = (mempty, mempty, r)

instance (Monoid b, Monoid c, Monoid d) => Injects r_ m (r_ m, b, c, d) where
  inject r = (r, mempty, mempty, mempty)

instance (Monoid a, Monoid c, Monoid d) => Injects r_ m (a, r_ m, c, d) where
  inject r = (mempty, r, mempty, mempty)

instance (Monoid a, Monoid b, Monoid d) => Injects r_ m (a, b, r_ m, d) where
  inject r = (mempty, mempty, r, mempty)

instance (Monoid a, Monoid b, Monoid c) => Injects r_ m (a, b, c, r_ m) where
  inject r = (mempty, mempty, mempty, r)


-- | Mirror image of 'Dep.Has.HasAll'.
type InjectsAll :: [(Type -> Type) -> Type] -> (Type -> Type) -> Type -> Constraint
type family InjectsAll rs_ m e where
  InjectsAll '[] m e = ()
  InjectsAll (r_ : rs_) m e = (Injects r_ m e, InjectsAll rs_ m e)