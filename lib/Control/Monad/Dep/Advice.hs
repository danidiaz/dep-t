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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}

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
type Capable ::
  (Type -> (Type -> Type) -> Constraint) ->
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Constraint
type Capable c e m = (c (e (DepT e m)) (DepT e m), Monad m)

type Advice ::
  (Type -> Constraint) ->
  (Type -> (Type -> Type) -> Constraint) ->
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type
data Advice ac c e m = Advice {
    tweakArgs :: forall as . (All ac as, Capable c e m) => 
        NP I as -> DepT e m (NP I as),
    tweakExecution :: 
        forall x.
        Capable c e m =>
        DepT e m x ->
        DepT e m x
    }

-- A function can be an advisee if it's multicurryable, 
-- and the list of arguments, the return type, and the environment, satisfy some requisites.
type Advisee ::
  (Type -> Constraint) ->
  (Type -> (Type -> Type) -> Constraint) ->
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type ->
  Constraint
class Advisee ac c e m r | r -> e m where
  give :: Advice ac c e m -> r -> r

-- this class is for decomposing I think. It should ignore all constraints.
-- do we need to include e and m here?
class Multicurryable as terminal r | r -> as terminal where
    multiuncurry :: r -> NP I as -> terminal
    multicurry :: (NP I as -> terminal) -> r
    
instance Multicurryable '[] (DepT e m x) (DepT e m x) where
    multiuncurry action Nil = action
    multicurry f = f Nil 

instance Multicurryable as terminal r => Multicurryable (a ': as) terminal (a -> r) where
    multiuncurry f (I a :* as) = multiuncurry @as @terminal @r (f a) as
    multicurry f a = multicurry @as @terminal @r (f . (:*) (I a))

-- instance (Capable c e m) => Advisee ac c e m (DepT e m x) where
--   give (Advice {tweakArgs,tweakExecution}) advisee = 
--     do _ <- tweakArgs Nil
--        tweakExecution advisee 

-- The advice shouldn't care about the as! At least in the definition.
-- But the advisee typeclass *should care*
-- One typeclass to go backwards and forwards?
-- Uncurry typeclass?
-- The terminal case *doesn't know* how many previous parameters there have been.
-- extra parameter, start with '[] to signify "this is the beginning of the function" ?

-- instance (Advisee ac c e m r, ac a) => Advisee ac c e m (a -> r) where
--   give (Advice {tweakArgs,tweakExecution}) (f :: a -> r) a =
--     give @ac @c @e @m @r (Advice (\args -> tweakArgs (args)) tweakExecution) (f a)
--     -- give @ac @c @e @m @r (Advice (\args d -> advice (I a :* args) d)) (f a)

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
