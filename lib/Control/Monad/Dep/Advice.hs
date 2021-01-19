{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Monad.Dep.Advice
  ( Advice (..),
    advise,
    Capable,
    EnvTop,
    EnvAnd,
    EnvEq,
    MonadConstraint,
    ArgTop,
    ResTop,
    -- * sop-core re-exports
    Top,
    All,
    And,
    NP(..),
    I(..),
    cfoldMap_NP,
  )
where

import Control.Monad.Dep
import Data.Constraint
import Data.Kind
import Data.SOP
import Data.SOP.NP

--
--
--
type Capable ::
  (Type -> (Type -> Type) -> Constraint) ->
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Constraint

type Capable c e m = (c (e (DepT e m)) (DepT e m), Monad m)

-- type Advice ::
--   (Type -> Constraint) ->
--   (Type -> (Type -> Type) -> Constraint) ->
--   ((Type -> Type) -> Type) ->
--   (Type -> Type) ->
--   Type
data Advice ca cem cr where
  Advice ::
    forall u ca cem cr.
    Proxy u ->
    ( forall as e m.
      (All ca as, Capable cem e m) =>
      NP I as ->
      DepT e m (u,NP I as)
    ) ->
    ( forall e m r.
      (Capable cem e m, cr r) =>
      u ->
      DepT e m r ->
      DepT e m r
    ) ->
    Advice ca cem cr

data Pair a b = Pair !a !b

-- |
--    The first advice is the "outer" one. It gets executed first on the way of
--    calling the advised function, and last on the way out of the function.

-- But what about the order of argument manipulation? I'm not sure...
instance Semigroup (Advice ca cem cr) where
  Advice outer tweakArgsOuter tweakExecutionOuter <> Advice inner tweakArgsInner tweakExecutionInner =
    let captureExistentials ::
          forall ca cem cr outer inner.
          Proxy outer ->
          ( forall as e m.
            (All ca as, Capable cem e m) =>
            NP I as ->
            DepT e m (outer,NP I as)
          ) ->
          ( forall e m r.
            (Capable cem e m, cr r) =>
            outer ->
            DepT e m r ->
            DepT e m r
          ) ->
          Proxy inner ->
          ( forall as e m.
            (All ca as, Capable cem e m) =>
            NP I as ->
            DepT e m (inner,NP I as)
          ) ->
          ( forall e m r.
            (Capable cem e m, cr r) =>
            inner ->
            DepT e m r ->
            DepT e m r
          ) ->
          Advice ca cem cr
        captureExistentials _ tweakArgsOuter' tweakExecutionOuter' _ tweakArgsInner' tweakExecutionInner' =
          Advice @(Pair outer inner) @ca @cem @cr
            (Proxy @(Pair outer inner))
            ( let tweakArgs ::
                    forall as e m.
                    (All ca as, Capable cem e m) =>
                    NP I as ->
                    DepT e m (Pair outer inner,NP I as)
                  tweakArgs args =
                    do
                      (uOuter, argsOuter) <- tweakArgsOuter' @as @e @m args
                      (uInner, argsInner) <- tweakArgsInner' @as @e @m argsOuter
                      pure (Pair uOuter uInner, argsInner)
               in tweakArgs
            )
            ( let tweakExecution ::
                    forall e m r.
                    (Capable cem e m, cr r) =>
                    Pair outer inner ->
                    DepT e m r ->
                    DepT e m r
                  tweakExecution =
                    ( \(Pair uOuter uInner) action ->
                        tweakExecutionOuter' @e @m @r uOuter (tweakExecutionInner' @e @m @r uInner action)
                    )
               in tweakExecution
            )
     in captureExistentials @ca @cem @cr outer tweakArgsOuter tweakExecutionOuter inner tweakArgsInner tweakExecutionInner

instance Monoid (Advice ca cem cr) where
  mappend = (<>)
  mempty = Advice (Proxy @()) (\args -> pure (pure args)) (const id)

-- A function can be an advisee if it's multicurryable,
-- and the list of arguments, the return type, and the environment, satisfy some requisites.
-- type Advisee ::
--   (Type -> Constraint) ->
--   (Type -> (Type -> Type) -> Constraint) ->
--   (Type -> Constraint) ->
--   [Type] ->
--   ((Type -> Type) -> Type) ->
--   (Type -> Type) ->
--   Type ->
--   Type ->
--   Constraint
-- -- do we really need as e m r here, or could we go with the constraints only?
-- -- Perhaps using a type family? Would that be bad for inference?
-- class (Multicurryable as e m r advisee, All ca as, Capable cem e m, cr r) => Advisee ca cem cr as e m r advisee where
--   advise :: Advice ac cem cr -> advisee -> advisee

advise ::
  forall ca cem cr as e m r advisee.
  (Multicurryable as e m r advisee, All ca as, Capable cem e m, cr r) =>
  Advice ca cem cr ->
  advisee ->
  advisee
advise (Advice _ tweakArgs tweakExecution) advisee = do
  let uncurried = multiuncurry @as @e @m @r advisee
      uncurried' args = do
        (u,args') <- tweakArgs args
        tweakExecution u (uncurried args')
   in multicurry @as @e @m @r uncurried'

-- this class is for decomposing I think. It should ignore all constraints.
-- do we need to include e and m here?
type Multicurryable ::
  [Type] ->
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type ->
  Type ->
  Constraint
class Multicurryable as e m r curried | curried -> as e m r where
  multiuncurry :: curried -> NP I as -> DepT e m r
  multicurry :: (NP I as -> DepT e m r) -> curried

instance Multicurryable '[] e m r (DepT e m r) where
  multiuncurry action Nil = action
  multicurry f = f Nil

instance Multicurryable as e m r curried => Multicurryable (a ': as) e m r (a -> curried) where
  multiuncurry f (I a :* as) = multiuncurry @as @e @m @r @curried (f a) as
  multicurry f a = multicurry @as @e @m @r @curried (f . (:*) (I a))

-- instance (Capable cem e m, cr r) => Advisee ca cem cr '[] e m r (DepT e m r) where
--     advise (Advice {tweakArgs,tweakExecution}) advisee =
--       do _ <- tweakArgs Nil
--          tweakExecution advisee

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
-- class (f x, g x) => (f `ArgAnd` g) x
-- instance (f x, g x) => (f `ArgAnd` g) x

-- infixl 7 `ArgAnd`

type ArgTop x = Top x

-- type ArgAnd f g = And f g
-- infixl 7 `ArgAnd`

type ResTop x = Top x

-- type ResAnd f g = And f g
-- infixl 7 `ResAnd`

-- restrictRes :: forall more less ca cem . (forall r . less r :- more r) -> Advice ca cem less -> Advice ca cem more
-- restrictRes (Sub Dict) (Advice proxy tweakArgsOuter tweakExecutionOuter) = Advice proxy tweakArgsOuter tweakExecutionOuter
    

