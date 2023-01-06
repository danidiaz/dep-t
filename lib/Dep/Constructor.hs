{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
    -- * "Control.Arrow" re-exports
    arr
  )
where

import Control.Applicative
import Data.Bifunctor (second)
import Data.Coerce
import Data.Function (fix)
import Data.Kind
import Data.Typeable
import Dep.Env hiding (AccumConstructor, Constructor, accumConstructor, constructor, fixEnv, fixEnvAccum)
import Control.Category (Category)
import Control.Category qualified
import Control.Arrow

-- | A phase with the effect of \"constructing each component by reading its
-- dependencies from a completed environment\". It should be the final phase.
--
-- The 'Constructor' phase for an environment will typically be parameterized
-- with the environment itself.
newtype Constructor (deps :: Type) component
  = Constructor (deps -> component)
  deriving stock Functor

deriving newtype instance Category Constructor
-- | Mostly useful for 'arr', which builds a 'Constructor' out of a regular function.
deriving newtype instance Arrow Constructor
-- | 'pure' lifts a component that doesn't require any dependencies.
deriving newtype instance Applicative (Constructor deps)

-- | Turn an environment-consuming function into a 'Constructor' that can be slotted
-- into some field of a 'Phased' environment.
--
-- Equivalent to 'arr'.
constructor ::
  forall deps component.
  (deps -> component) ->
  Constructor deps component
-- same order of type parameters as Has
constructor = Constructor

-- | A generalized 'Constructor' which produces, in addition to the result
-- value, an @accum@ value which is then aggregated across all components and fed
-- back along with the completed environment.
--
-- Like 'Constructor', 'AccumConstructor' should be the final phase.
newtype AccumConstructor (accum :: Type) (deps :: Type) component
  = AccumConstructor ((accum, deps) -> (accum, component))
  deriving stock Functor

-- | 'pure' lifts a component that doesn't require any dependencies.
-- The produced accumulator will be 'mempty'.
instance Monoid accum => Applicative (AccumConstructor accum deps) where
  pure component = _accumConstructor_ \_ -> component
  liftA2 f (AccumConstructor u) (AccumConstructor v) = AccumConstructor \accumdeps ->
    let (acc1, component1) = u accumdeps
        (acc2, component2) = v accumdeps
     in (acc1 <> acc2, f component1 component2)

-- |
instance Monoid accum => Category (AccumConstructor accum) where
  id = _accumConstructor_ id
  (.) (AccumConstructor f) (AccumConstructor g) = AccumConstructor \(~(accum0,deps0)) -> 
      let (accum1, deps1) = g (accum0,deps0)
          (accum2, deps2) = f (accum0,deps1)
       in (accum1 <> accum2, deps2)

-- | Mostly useful for 'arr', which builds an 'AccumConstructor' out of a regular function. The produced accumulator will be 'mempty'.
instance Monoid accum => Arrow (AccumConstructor accum) where
  arr = _accumConstructor_
  first (AccumConstructor f) = AccumConstructor \(~(accum,(deps,extra))) -> 
    let (accum', component) = f (accum,deps)
     in (accum', (component, extra))

-- | Turn an environment-consuming function into an 'AccumConstructor' that can
-- be slotted into some field of a 'Phased' environment. The function also
-- consumes and produces a monoidal accumulator.
accumConstructor ::
  forall accum deps component.
  (accum -> deps -> (accum, component)) ->
  AccumConstructor accum deps component
accumConstructor f = AccumConstructor (\(~(accum, deps)) -> f accum deps)

accumConstructor_ ::
  forall accum deps component.
  Monoid accum =>
  -- | Consumes the accumulator but doesn't produce it (returns the 'mempty' accumulator.)
  (accum -> deps -> component) ->
  AccumConstructor accum deps component
accumConstructor_ f = accumConstructor $ \accum deps -> (mempty, f accum deps)

_accumConstructor ::
  forall accum deps component.
  -- | Doesn't consume the accumulator but produces it.
  (deps -> (accum, component)) ->
  AccumConstructor accum deps component
_accumConstructor f = accumConstructor $ \_ deps -> f deps

-- | Equivalent to 'arr'.
_accumConstructor_ ::
  forall accum deps component.
  Monoid accum =>
  -- | Neither consumes nor produces the accumulator, like a 'Constructor'.
  (deps -> component) ->
  AccumConstructor accum deps component
_accumConstructor_ f = accumConstructor $ \_ deps -> (mempty, f deps)

-- | This is a method of performing dependency injection by building fixpoints.
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
-- We might have arrived as this \"ready-to-wire\" environment by peeling away
-- successive layers of applicative functor composition using 'pullPhase', until
-- only the wiring phase remains.
--
--  >>> :{
--  newtype Foo d = Foo {foo :: String -> d ()} deriving Generic
--  newtype Bar d = Bar {bar :: String -> d ()} deriving Generic
--  makeIOFoo :: MonadIO m => Foo m
--  makeIOFoo = Foo (liftIO . putStrLn)
--  makeBar :: Has Foo m env => env -> Bar m
--  makeBar (asCall -> call) = Bar (call foo)
--  type Deps_ = InductiveEnv [Bar,Foo]
--  type Deps = Deps_ Identity
--  deps_ :: Deps_ (Constructor (Deps IO)) IO
--  deps_ = EmptyEnv
--      & AddDep @Foo (constructor (\_ -> makeIOFoo))
--      & AddDep @Bar (constructor makeBar)
--  deps :: Deps IO
--  deps = fixEnv deps_
-- :}
--
-- >>> :{
--  bar (dep deps) "this is bar"
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

-- | A generalized 'fixEnv' which threads a monoidal accumulator
-- along with the environment.
--
-- Sometimes, we need constructors to produce a monoidal value along with the
-- component. Think for example about some kind of composable startup action for
-- the component.
--
-- And on the input side, some constructors need access to the monoidal value
-- accumulated across all components. Think for example about a component which
-- publishes diagnostics coming from all other components.
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
