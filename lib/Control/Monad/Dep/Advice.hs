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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Monad.Dep.Advice (
        Advisee(..),
        EnvTop,
        EnvAnd,
        EnvEq,
        MonadConstraint,
        ArgAnd
    ) where

import Control.Monad.Dep
import Data.Kind
import Data.SOP.NP


--
--
--
type Advisee ::
  (Type -> Constraint) ->
  Type ->
  (Type -> (Type -> Type) -> Constraint) ->
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type ->
  Constraint
class Advisee ac u c e m r | r -> e m where
  advise ::
    ( forall a.
      ac a =>
      a ->
      u
    ) ->
    ( forall x.
      (c (e (DepT e m)) (DepT e m), Monad m) =>
      [u] ->
      DepT e m x ->
      DepT e m x
    ) ->
    r ->
    r

instance (c (e (DepT e m)) (DepT e m), Monad m) => Advisee ac u c e m (DepT e m x) where
  advise _ advice d = advice [] d

instance (Advisee ac u c e m r, ac a) => Advisee ac u c e m (a -> r) where
  advise argAdaptor advice ar =
    let advise' = advise @ac @u @c @e @m @r argAdaptor
     in \a -> advise' (\args d -> advice (argAdaptor a : args) d) (ar a)


{-| 
    A constraint which requires nothing of the environment and the associated monad.

    Pass this with a type application to 'advise' and 'advise' when no constraint is needed.

    The @-Top@ and @-And@ constraints have been lifted from the @Top@ and @And@ constraints from sop-core.
-}
type EnvTop :: (Type -> (Type -> Type) -> Constraint)
class EnvTop e m
instance EnvTop e m

{-| 
    Creates composite constraints on the environment and monad. 

    For example, an advice which requires both a @HasLogger@ and a
    @HasRepository@ migh use this.
 -}
type EnvAnd :: (Type -> (Type -> Type) -> Constraint) -> (Type -> (Type -> Type) -> Constraint) ->  (Type -> (Type -> Type) -> Constraint)
class (f e m, g e m) => (f `EnvAnd` g) e m 
instance (f e m, g e m) => (f `EnvAnd` g) e m
infixl 7 `EnvAnd`

{-| 
    Useful when whe don't want to instrument some generic environment, but a
    concrete one, with direct access to all fields and all that.
 -}
type EnvEq :: Type -> (Type -> Type) -> Type -> (Type -> Type) -> Constraint
class (c' ~ c, m' ~ m) => EnvEq c' m' c m
instance (c' ~ c, m' ~ m) => EnvEq c' m' c m

{-| 
    Allows us to require a constraint only on the monad. Useful for requiring @MonadIO@ for example.
 -}
type MonadConstraint :: ((Type -> Type) -> Constraint) -> (Type -> (Type -> Type) -> Constraint)
class c m => MonadConstraint c e m
instance c m => MonadConstraint c e m

{-|
    For use in the—likely very rare—case in which `advise` needs two
    constraints on the advisee's arguments.
 -}
class (f x, g x) => (f `ArgAnd` g) x
instance (f x, g x) => (f `ArgAnd` g) x
infixl 7 `ArgAnd`

