{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}

module Dep.Constructor
  ( -- * Constructor phase
    Constructor,
    constructor,
    lmapConstructor,
    fixEnv,

    -- * Constructor with accumulator
    AccumConstructor,
    accumConstructor,
    accumConstructor_,
    _accumConstructor,
    _accumConstructor_,
    lmapAccumConstructor,
    fixEnvAccum,
  )
where

import Control.Applicative
import Data.Bifunctor (second)
import Data.Coerce
import Data.Function (fix)
import Data.Kind
import Data.Typeable
import Dep.Env hiding (AccumConstructor, Constructor, accumConstructor, constructor, fixEnv, fixEnvAccum)

-- | A phase with the effect of \"constructing each component by reading its
-- dependencies from a completed environment\". It should be the final phase.
--
-- The 'Constructor' phase for an environment will typically be parameterized
-- with the environment itself.
newtype Constructor (deps :: Type) component
  = Constructor (deps -> component)
  deriving stock Functor
  deriving newtype Applicative

-- | Turn an environment-consuming function into a 'Constructor' that can be slotted
-- into some field of a 'Phased' environment.
constructor ::
  forall deps component.
  (deps -> component) ->
  Constructor deps component
-- same order of type parameters as Has
constructor = Constructor

-- | A generalization of 'Constructor' which produces, in addition to the result
-- value, a value @w@ which is then aggregated across all components and fed
-- back along with the completed environment.
--
-- Like 'Constructor', 'AccumConstructor' should be the final phase.
newtype AccumConstructor (accum :: Type) (deps :: Type) component
  = AccumConstructor ((accum, deps) -> (accum, component))
  deriving stock Functor

instance Monoid accum => Applicative (AccumConstructor accum deps) where
  pure component = _accumConstructor_ \_ -> component
  liftA2 f (AccumConstructor u) (AccumConstructor v) = AccumConstructor \accumdeps ->
    let (acc1, component1) = u accumdeps
        (acc2, component2) = v accumdeps
     in (acc1 <> acc2, f component1 component2)

-- | Turn an environment-consuming function into a 'Constructor' that can be slotted
-- into some field of a 'Phased' environment.
accumConstructor ::
  forall accum deps component.
  (accum -> deps -> (accum, component)) ->
  AccumConstructor accum deps component
accumConstructor f = AccumConstructor (\(~(accum, deps)) -> f accum deps)

accumConstructor_ ::
  forall accum deps component.
  Monoid accum =>
  -- | Consumes the accumulator but doesn't produce it.
  (accum -> deps -> component) ->
  AccumConstructor accum deps component
accumConstructor_ f = accumConstructor $ \accum deps -> (mempty, f accum deps)

_accumConstructor ::
  forall accum deps component.
  -- | Doesn't consume the accumulator but produces it.
  (deps -> (accum, component)) ->
  AccumConstructor accum deps component
_accumConstructor f = accumConstructor $ \_ deps -> f deps

_accumConstructor_ ::
  forall accum deps component.
  Monoid accum =>
  -- | Neither consumes nor produces the accumulator, like a 'Constructor'.
  (deps -> component) ->
  AccumConstructor accum deps component
_accumConstructor_ f = accumConstructor $ \_ deps -> (mempty, f deps)

-- | This is a method of performing dependency injection that doesn't require
-- "Control.Monad.Dep.DepT" at all. In fact, it doesn't require the use of
-- /any/ monad transformer!
--
-- If we have a environment whose fields are functions that construct each
-- component by searching for its dependencies in a \"fully built\" version of
-- the environment, we can \"tie the knot\" to obtain the \"fully built\"
-- environment. This works as long as there aren't any circular dependencies
-- between components.
--
-- Think of it as a version of 'Data.Function.fix' that, instead of \"tying\" a single
-- function, ties a whole record of them.
--
-- The @env_ (Constructor (env_ Identity m)) m@ parameter might be the result of peeling
-- away successive layers of applicative functor composition using 'pullPhase',
-- until only the wiring phase remains.
--
--  >>> :{
--  newtype Foo d = Foo {foo :: String -> d ()} deriving Generic
--  newtype Bar d = Bar {bar :: String -> d ()} deriving Generic
--  makeIOFoo :: MonadIO m => Foo m
--  makeIOFoo = Foo (liftIO . putStrLn)
--  makeBar :: Has Foo m env => env -> Bar m
--  makeBar (asCall -> call) = Bar (call foo)
--  env :: InductiveEnv [Bar,Foo] (Constructor (InductiveEnv [Bar,Foo] Identity IO)) IO
--  env = EmptyEnv
--      & AddDep @Foo (constructor (\_ -> makeIOFoo))
--      & AddDep @Bar (constructor makeBar)
--  envReady :: InductiveEnv [Bar,Foo] Identity IO
--  envReady = fixEnv env
-- :}
--
-- >>> :{
--  bar (dep envReady) "this is bar"
-- :}
-- this is bar
fixEnv ::
  (Phased env_, Typeable env_, Typeable m) =>
  -- | Environment where each field is wrapped in a 'Constructor'
  env_ (Constructor (env_ Identity m)) m ->
  -- | Fully constructed environment, ready for use.
  env_ Identity m
fixEnv env = fix (pullPhase (liftAH decompose env))
  where
    decompose (Constructor f) = coerce f

-- | A generalized version of 'fixEnv' which threads a monoidal accumulator
-- along with the environment.
--
-- Sometimes, we need constructors to produce a monoidal value along with the
-- component. Think for example about some kind of composable startup action for
-- the component.
--
-- And on the input side, some constructors need access to the monoidal value
-- accumulated across all components. Think for example about a component which
-- publishes accumulated diagnostics coming from all other components.
fixEnvAccum ::
  (Phased env_, Typeable env_, Typeable m, Monoid accum, Typeable accum) =>
  -- | Environment where each field is wrapped in an 'AccumConstructor'
  env_ (AccumConstructor accum (env_ Identity m)) m ->
  -- | Fully constructed accumulator and environment, ready for use.
  (accum, env_ Identity m)
fixEnvAccum env =
  let f = pullPhase <$> pullPhase (liftAH decompose env)
   in fix f
  where
    decompose (AccumConstructor f) = coerce f

-- | Change the dependency environment seen by the component.
lmapConstructor ::
  forall deps deps' component.
  Typeable component =>
  -- | Modifies the environment, with access to the 'TypeRep' of the component.
  (TypeRep -> deps -> deps') ->
  Constructor deps' component ->
  Constructor deps component
lmapConstructor tweak (Constructor f) =
  let tyRep = typeRep (Proxy @component)
   in Constructor $ f . tweak tyRep

-- | Change the dependency environment seen by the component.
--
-- The accumulator remains unchanged.
lmapAccumConstructor ::
  forall accum deps deps' component.
  Typeable component =>
  -- | Modifies the environment, with access to the 'TypeRep' of the component.
  (TypeRep -> deps -> deps') ->
  AccumConstructor accum deps' component ->
  AccumConstructor accum deps component
lmapAccumConstructor tweak (AccumConstructor f) =
  let tyRep = typeRep (Proxy @component)
   in AccumConstructor (\(~(accum, deps)) -> f (accum, tweak tyRep deps))

-- $setup
--
-- >>> :set -XTypeApplications
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XImportQualifiedPost
-- >>> :set -XTemplateHaskell
-- >>> :set -XStandaloneKindSignatures
-- >>> :set -XNamedFieldPuns
-- >>> :set -XFunctionalDependencies
-- >>> :set -XFlexibleContexts
-- >>> :set -XDataKinds
-- >>> :set -XBlockArguments
-- >>> :set -XFlexibleInstances
-- >>> :set -XTypeFamilies
-- >>> :set -XDeriveGeneric
-- >>> :set -XViewPatterns
-- >>> :set -XDerivingStrategies
-- >>> :set -XDerivingVia
-- >>> :set -XDeriveAnyClass
-- >>> :set -XStandaloneDeriving
-- >>> :set -XUndecidableInstances
-- >>> :set -XTypeOperators
-- >>> :set -XScopedTypeVariables
-- >>> import Data.Kind
-- >>> import Data.Function ((&))
-- >>> import Control.Monad.IO.Class
-- >>> import Dep.Has
-- >>> import Dep.Env hiding (AccumConstructor, Constructor, accumConstructor, constructor, fixEnv, fixEnvAccum)
-- >>> import GHC.Generics (Generic)
