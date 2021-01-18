{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Monad.Dep.Advice
  ( Advisee (..),
    EnvTop,
    EnvAnd,
    EnvEq,
    MonadConstraint,
    ArgAnd,
  )
where

import Control.Monad.Dep
import Data.Kind
import Data.SOP

--
--
--
type Advice ::
  (Type -> Constraint) ->
  (Type -> (Type -> Type) -> Constraint) ->
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type
newtype Advice ac c e m = Advice
    ( forall as x.
      (All ac as, c (e (DepT e m)) (DepT e m), Monad m) =>
      NP I as ->
      DepT e m x ->
      DepT e m x)

type Advisee ::
  (Type -> Constraint) ->
  (Type -> (Type -> Type) -> Constraint) ->
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type ->
  Constraint
class Advisee ac c e m r | r -> e m where
  give :: Advice ac c e m -> r -> r

instance (c (e (DepT e m)) (DepT e m), Monad m) => Advisee ac c e m (DepT e m x) where
  give (Advice advice) d = advice Nil d

instance (Advisee ac c e m r, ac a) => Advisee ac c e m (a -> r) where
  give (Advice advice) (f :: a -> r) a =
    give @ac @c @e @m @r (Advice (\args d -> advice (I a :* args) d)) (f a)

-- |
--    A constraint which requires nothing of the environment and the associated monad.
--
--    Pass this with a type application to 'advise' and 'advise' when no constraint is needed.
--
--    The @-Top@ and @-And@ constraints have been lifted from the @Top@ and @And@ constraints from sop-core.
type EnvTop :: (Type -> (Type -> Type) -> Constraint)
class EnvTop e m

instance EnvTop e m

-- |
--    Creates composite constraints on the environment and monad.
--
--    For example, an advice which requires both a @HasLogger@ and a
--    @HasRepository@ migh use this.
type EnvAnd :: (Type -> (Type -> Type) -> Constraint) -> (Type -> (Type -> Type) -> Constraint) -> (Type -> (Type -> Type) -> Constraint)
class (f e m, g e m) => (f `EnvAnd` g) e m

instance (f e m, g e m) => (f `EnvAnd` g) e m

infixl 7 `EnvAnd`

-- |
--    Useful when whe don't want to instrument some generic environment, but a
--    concrete one, with direct access to all fields and all that.
type EnvEq :: Type -> (Type -> Type) -> Type -> (Type -> Type) -> Constraint
class (c' ~ c, m' ~ m) => EnvEq c' m' c m

instance (c' ~ c, m' ~ m) => EnvEq c' m' c m

-- |
--    Allows us to require a constraint only on the monad. Useful for requiring @MonadIO@ for example.
type MonadConstraint :: ((Type -> Type) -> Constraint) -> (Type -> (Type -> Type) -> Constraint)
class c m => MonadConstraint c e m

instance c m => MonadConstraint c e m

-- |
--    For use in the—likely very rare—case in which `advise` needs two
--    constraints on the advisee's arguments.
class (f x, g x) => (f `ArgAnd` g) x

instance (f x, g x) => (f `ArgAnd` g) x

infixl 7 `ArgAnd`
