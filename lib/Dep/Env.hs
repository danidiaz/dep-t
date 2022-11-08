{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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

-- | This module provides helpers for building dependency injection
-- environments composed of records.
--
-- It's not necessary when defining the record components themselves, in that
-- case "Dep.Has" should suffice.
--
-- >>> :{
-- type Logger :: (Type -> Type) -> Type
-- newtype Logger d = Logger {
--     info :: String -> d ()
--   }
-- --
-- data Repository d = Repository
--   { findById :: Int -> d (Maybe String)
--   , putById :: Int -> String -> d ()
--   , insert :: String -> d Int
--   }
-- --
-- data Controller d = Controller
--   { create :: d Int
--   , append :: Int -> String -> d Bool
--   , inspect :: Int -> d (Maybe String)
--   }
-- --
-- type EnvHKD :: (Type -> Type) -> (Type -> Type) -> Type
-- data EnvHKD h m = EnvHKD
--   { logger :: h (Logger m),
--     repository :: h (Repository m),
--     controller :: h (Controller m)
--   } deriving stock Generic
--     deriving anyclass (FieldsFindableByType, DemotableFieldNames, Phased)
-- deriving via Autowired (EnvHKD Identity m) instance Autowireable r_ m (EnvHKD Identity m) => Has r_ m (EnvHKD Identity m)
-- :}
--
--
-- The module also provides a monad transformer-less way of performing dependency
-- injection, by means of 'fixEnv'.
module Dep.Env
  ( -- * A general-purpose Has
    Has,

    -- * Helpers for deriving Has

    -- ** via the default field name
    TheDefaultFieldName (..),

    -- ** via arbitrary field name
    TheFieldName (..),

    -- ** via autowiring
    FieldsFindableByType (..),
    Autowired (..),
    Autowireable,

    -- * Managing phases
    Phased (..),
    pullPhase,
    mapPhase,
    liftA2Phase,

    -- ** Working with field names
    DemotableFieldNames (..),
    demoteFieldNames,
    mapPhaseWithFieldNames,

    -- ** Constructing phases
    -- $phasehelpers
    bindPhase,
    skipPhase,
    -- $phasehelpers2
    Bare,
    fromBare,
    toBare,

    -- * Injecting dependencies by tying the knot
    fixEnv,
    Constructor,
    constructor,
    fixEnvAccum,
    AccumConstructor,

    -- * Inductive environment with anonymous fields
    InductiveEnv (..),
    addDep,
    emptyEnv,

    -- * Re-exports
    Identity (..),
    Constant (..),
    Compose (..),
  )
where

import Control.Applicative
import Data.Coerce
import Data.Function (fix)
import Data.Functor (($>), (<&>))
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Data.String
import Data.Type.Equality (type (==))
import Data.Typeable
import Dep.Has
import GHC.Generics qualified as G
import GHC.Records
import GHC.TypeLits

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
-- >>> import Dep.Env
-- >>> import Dep.Env
-- >>> import GHC.Generics (Generic)

-- via the default field name

-- | Helper for @DerivingVia@ 'HasField' instances.
--
-- It expects the component to have as field name the default fieldname
-- specified by 'Dep'.
--
-- This is the same behavior as the @DefaultSignatures@ implementation for
-- 'Has', so maybe it doesn't make much sense to use it, except for
-- explicitness.
newtype TheDefaultFieldName (env :: Type) = TheDefaultFieldName env

instance
  (Dep r_, HasField (DefaultFieldName r_) (env_ m) u, Coercible u (r_ m)) =>
  Has r_ m (TheDefaultFieldName (env_ m))
  where
  dep (TheDefaultFieldName env) = coerce . getField @(DefaultFieldName r_) $ env

-- | Helper for @DerivingVia@ 'HasField' instances.
--
-- The field name is specified as a 'Symbol'.
type TheFieldName :: Symbol -> Type -> Type
newtype TheFieldName (name :: Symbol) (env :: Type) = TheFieldName env

instance
  (HasField name (env_ m) u, Coercible u (r_ m)) =>
  Has r_ m (TheFieldName name (env_ m))
  where
  dep (TheFieldName env) = coerce . getField @name $ env

-- via autowiring

-- | Class for getting the field name from the field's type.
--
-- The default implementation of 'FindFieldByType' requires a 'G.Generic'
-- instance, but users can write their own implementations.
type FieldsFindableByType :: Type -> Constraint
class FieldsFindableByType (env :: Type) where
  type FindFieldByType env (r :: Type) :: Symbol
  type FindFieldByType env r = FindFieldByType_ env r

-- | Helper for @DerivingVia@ 'HasField' instances.
--
-- The fields are identified by their types.
--
-- It uses 'FindFieldByType' under the hood.
--
-- __BEWARE__: for large records with many components, this technique might
-- incur in long compilation times.
type Autowired :: Type -> Type
newtype Autowired (env :: Type) = Autowired env

-- | Constraints required when @DerivingVia@ /all/ possible instances of 'Has' in
-- a single definition.
--
-- This only works for environments where all the fields come wrapped in
-- "Data.Functor.Identity".
type Autowireable r_ (m :: Type -> Type) (env :: Type) = HasField (FindFieldByType env (r_ m)) env (Identity (r_ m))

instance
  ( FieldsFindableByType (env_ m),
    HasField (FindFieldByType (env_ m) (r_ m)) (env_ m) u,
    Coercible u (r_ m)
  ) =>
  Has r_ m (Autowired (env_ m))
  where
  dep (Autowired env) = coerce @u $ getField @(FindFieldByType (env_ m) (r_ m)) env

type FindFieldByType_ :: Type -> Type -> Symbol
type family FindFieldByType_ env r where
  FindFieldByType_ env r = IfMissing r (GFindFieldByType (ExtractProduct (G.Rep env)) r)

type ExtractProduct :: (k -> Type) -> k -> Type
type family ExtractProduct envRep where
  ExtractProduct (G.D1 _ (G.C1 _ z)) = z

type IfMissing :: Type -> Maybe Symbol -> Symbol
type family IfMissing r ms where
  IfMissing r Nothing =
    TypeError
      ( Text "The component "
          :<>: ShowType r
          :<>: Text " could not be found in environment."
      )
  IfMissing _ (Just name) = name

-- The k -> Type alwasy trips me up
type GFindFieldByType :: (k -> Type) -> Type -> Maybe Symbol
type family GFindFieldByType r x where
  GFindFieldByType (left G.:*: right) r =
    WithLeftResult_ (GFindFieldByType left r) right r
  GFindFieldByType (G.S1 (G.MetaSel ('Just name) _ _ _) (G.Rec0 r)) r = Just name
  -- Here we are saying "any wrapper whatsoever over r". Too general?
  -- If the wrapper is not coercible to the underlying r, we'll fail later.
  GFindFieldByType (G.S1 (G.MetaSel ('Just name) _ _ _) (G.Rec0 (_ r))) r = Just name
  GFindFieldByType _ _ = Nothing

type WithLeftResult_ :: Maybe Symbol -> (k -> Type) -> Type -> Maybe Symbol
type family WithLeftResult_ leftResult right r where
  WithLeftResult_ ('Just ls) right r = 'Just ls
  WithLeftResult_ Nothing right r = GFindFieldByType right r

--
--
-- Managing Phases

-- see also https://github.com/haskell/cabal/issues/7394#issuecomment-861767980

-- | Class of 2-parameter environments for which the first parameter @h@ wraps
-- each field and corresponds to phases in the construction of the environment,
-- and the second parameter @m@ is the effect monad used by each component.
--
-- @h@ will typically be a composition of applicative functors, each one
-- representing a phase. We advance through the phases by \"pulling out\" the
-- outermost phase and running it in some way, until we are are left with a
-- 'Constructor' phase, which we can remove using 'fixEnv'.
--
-- 'Phased' resembles [FunctorT, TraversableT and ApplicativeT](https://hackage.haskell.org/package/barbies-2.0.3.0/docs/Data-Functor-Transformer.html) from the [barbies](https://hackage.haskell.org/package/barbies) library. 'Phased' instances can be written in terms of them.
type Phased :: ((Type -> Type) -> (Type -> Type) -> Type) -> Constraint
class Phased (env_ :: (Type -> Type) -> (Type -> Type) -> Type) where
  -- | Used to implement 'pullPhase' and 'mapPhase',  typically you should use those functions instead.
  traverseH ::
    forall
      (h :: Type -> Type)
      (f :: Type -> Type)
      (g :: Type -> Type)
      (m :: Type -> Type).
    ( 
      Applicative f,
      Typeable f,
      Typeable g,
      Typeable h,
      Typeable m
    ) =>
    -- | Transform to be applied to each field.
    (forall x. Typeable x => h x -> f (g x)) ->
    env_ h m ->
    f (env_ g m)
  default traverseH ::
    forall
      (h :: Type -> Type)
      (f :: Type -> Type)
      (g :: Type -> Type)
      (m :: Type -> Type).
    ( Applicative f,
      Typeable f,
      Typeable g,
      Typeable h,
      Typeable m,
      G.Generic (env_ h m),
      G.Generic (env_ g m),
      GTraverseH h g (G.Rep (env_ h m)) (G.Rep (env_ g m))
    ) =>
    -- | Transform to be applied to each field.
    (forall x. Typeable x => h x -> f (g x)) ->
    env_ h m ->
    f (env_ g m)
  traverseH t env = G.to <$> gTraverseH t (G.from env)

  -- | Used to implement 'liftA2Phase', typically you should use that function instead.
  liftA2H ::
    forall (a :: Type -> Type) (f :: Type -> Type) (f' :: Type -> Type) (m :: Type -> Type).
    ( Typeable a,
      Typeable f,
      Typeable f',
      Typeable m
    ) =>
    -- | Binary operation to combine corresponding fields.
    (forall x. Typeable x => a x -> f x -> f' x) ->
    env_ a m ->
    env_ f m ->
    env_ f' m
  default liftA2H ::
    forall (a :: Type -> Type) (f :: Type -> Type) (f' :: Type -> Type) m.
    ( Typeable a,
      Typeable f,
      Typeable f',
      Typeable m,
      G.Generic (env_ a m),
      G.Generic (env_ f m),
      G.Generic (env_ f' m),
      GLiftA2Phase a f f' (G.Rep (env_ a m)) (G.Rep (env_ f m)) (G.Rep (env_ f' m))
    ) =>
    -- | Transform to be applied to each field.
    (forall x. Typeable x => a x -> f x -> f' x) ->
    env_ a m ->
    env_ f m ->
    env_ f' m
  liftA2H f enva env = G.to (gLiftA2Phase f (G.from enva) (G.from env))

-- | Take the outermost phase wrapping each component and \"pull it outwards\",
-- aggregating the phase's applicative effects.
--
-- >>> :{
--  newtype Foo d = Foo {foo :: String -> d ()} deriving Generic
--  makeIOFoo :: MonadIO m => Foo m
--  makeIOFoo = Foo (liftIO . putStrLn)
--  env :: InductiveEnv '[Foo] (IO `Compose` Constructor (InductiveEnv '[Foo] Identity IO)) IO
--  env = EmptyEnv
--      & AddDep @Foo (putStrLn "io phase" `bindPhase` \() -> constructor (\_ -> makeIOFoo))
--  ioOutside :: IO (InductiveEnv '[Foo] (Constructor (InductiveEnv '[Foo] Identity IO)) IO)
--  ioOutside = pullPhase env
-- :}
pullPhase ::
  forall (f :: Type -> Type) (g :: Type -> Type) (m :: Type -> Type) env_.
  (Phased env_, Applicative f, Typeable f, Typeable g, Typeable m) =>
  env_ (Compose f g) m ->
  -- Environment with the outer 'Applicative' layer pulled outward.
  f (env_ g m)
-- f first to help annotate the phase
pullPhase = traverseH @env_ getCompose

-- | Modify the outermost phase wrapping each component.
--
-- >>> :{
--  newtype Foo d = Foo {foo :: String -> d ()} deriving Generic
--  makeIOFoo :: MonadIO m => Foo m
--  makeIOFoo = Foo (liftIO . putStrLn)
--  env :: InductiveEnv '[Foo] ((,) Int `Compose` Constructor String) IO
--  env = EmptyEnv
--      & AddDep @Foo ((2,()) `bindPhase` \() -> constructor (\_ -> makeIOFoo))
--  env' :: InductiveEnv '[Foo] ((,) String `Compose` Constructor String) IO
--  env' = mapPhase (\(n,x) -> (show n,x)) env
-- :}
mapPhase ::
  forall (f :: Type -> Type) (f' :: Type -> Type) (g :: Type -> Type) (m :: Type -> Type) env_.
  (Phased env_, Typeable f, Typeable f', Typeable g, Typeable m) =>
  -- | Transform to be applied to each field.
  (forall x. Typeable x => f x -> f' x) ->
  env_ (Compose f g) m ->
  env_ (Compose f' g) m
-- f' first to help annotate the *target* of the transform?
mapPhase f env = runIdentity $ traverseH @env_ (\(Compose fg) -> Identity (Compose (f fg))) env

-- | Combine two environments with a function that works on their outermost phases.
liftA2Phase ::
  forall (a :: Type -> Type) (f' :: Type -> Type) (f :: Type -> Type) (g :: Type -> Type) (m :: Type -> Type) env_.
  (Phased env_, Typeable a, Typeable f, Typeable f', Typeable g, Typeable m) =>
  -- | Binary operation to combine corresponding fields.
  (forall x. Typeable x => a x -> f x -> f' x) ->
  env_ (Compose a g) m ->
  env_ (Compose f g) m ->
  env_ (Compose f' g) m
-- f' first to help annotate the *target* of the transform?
liftA2Phase f = liftA2H @env_ (\(Compose fa) (Compose fg) -> Compose (f fa fg))

class GTraverseH h g env env' | env -> h, env' -> g where
  gTraverseH :: Applicative f => (forall x. Typeable x => h x -> f (g x)) -> env x -> f (env' x)

instance
  (GTraverseH h g fields fields') =>
  GTraverseH
    h
    g
    (G.D1 metaData (G.C1 metaCons fields))
    (G.D1 metaData (G.C1 metaCons fields'))
  where
  gTraverseH t (G.M1 (G.M1 fields)) =
    G.M1 . G.M1 <$> gTraverseH @h @g t fields

instance
  ( GTraverseH h g left left',
    GTraverseH h g right right'
  ) =>
  GTraverseH h g (left G.:*: right) (left' G.:*: right')
  where
  gTraverseH t (left G.:*: right) =
    let left' = gTraverseH @h @g t left
        right' = gTraverseH @h @g t right
     in liftA2 (G.:*:) left' right'

instance
  Typeable bean =>
  GTraverseH
    h
    g
    (G.S1 metaSel (G.Rec0 (h bean)))
    (G.S1 metaSel (G.Rec0 (g bean)))
  where
  gTraverseH t (G.M1 (G.K1 hbean)) =
    G.M1 . G.K1 <$> t hbean

--
--
class GLiftA2Phase a f f' enva env env' | enva -> a, env -> f, env' -> f' where
  gLiftA2Phase :: (forall r. Typeable r => a r -> f r -> f' r) -> enva x -> env x -> env' x

instance
  GLiftA2Phase a f f' fieldsa fields fields' =>
  GLiftA2Phase
    a
    f
    f'
    (G.D1 metaData (G.C1 metaCons fieldsa))
    (G.D1 metaData (G.C1 metaCons fields))
    (G.D1 metaData (G.C1 metaCons fields'))
  where
  gLiftA2Phase f (G.M1 (G.M1 fieldsa)) (G.M1 (G.M1 fields)) =
    G.M1 (G.M1 (gLiftA2Phase @a @f @f' f fieldsa fields))

instance
  ( GLiftA2Phase a f f' lefta left left',
    GLiftA2Phase a f f' righta right right'
  ) =>
  GLiftA2Phase a f f' (lefta G.:*: righta) (left G.:*: right) (left' G.:*: right')
  where
  gLiftA2Phase f (lefta G.:*: righta) (left G.:*: right) =
    let left' = gLiftA2Phase @a @f @f' f lefta left
        right' = gLiftA2Phase @a @f @f' f righta right
     in (G.:*:) left' right'

instance
  Typeable bean =>
  GLiftA2Phase
    a
    f
    f'
    (G.S1 metaSel (G.Rec0 (a bean)))
    (G.S1 metaSel (G.Rec0 (f bean)))
    (G.S1 metaSel (G.Rec0 (f' bean)))
  where
  gLiftA2Phase f (G.M1 (G.K1 abean)) (G.M1 (G.K1 fgbean)) =
    G.M1 (G.K1 (f abean fgbean))

-- | Class of 2-parameter environments for which it's possible to obtain the
-- names of each field as values.
type DemotableFieldNames :: ((Type -> Type) -> (Type -> Type) -> Type) -> Constraint
class DemotableFieldNames env_ where
  demoteFieldNamesH :: (forall x. String -> h String x) -> env_ (h String) m
  default demoteFieldNamesH ::
    ( G.Generic (env_ (h String) m),
      GDemotableFieldNamesH h (G.Rep (env_ (h String) m))
    ) =>
    (forall x. String -> h String x) ->
    env_ (h String) m
  demoteFieldNamesH f = G.to (gDemoteFieldNamesH f)

-- | Bring down the field names of the environment to the term level and store
-- them in the accumulator of "Data.Functor.Constant".
demoteFieldNames :: forall env_ m. DemotableFieldNames env_ => env_ (Constant String) m
demoteFieldNames = demoteFieldNamesH Constant

class GDemotableFieldNamesH h env | env -> h where
  gDemoteFieldNamesH :: (forall x. String -> h String x) -> env x

instance
  GDemotableFieldNamesH h fields =>
  GDemotableFieldNamesH h (G.D1 metaData (G.C1 metaCons fields))
  where
  gDemoteFieldNamesH f = G.M1 (G.M1 (gDemoteFieldNamesH f))

instance
  ( GDemotableFieldNamesH h left,
    GDemotableFieldNamesH h right
  ) =>
  GDemotableFieldNamesH h (left G.:*: right)
  where
  gDemoteFieldNamesH f =
    gDemoteFieldNamesH f G.:*: gDemoteFieldNamesH f

instance KnownSymbol name => GDemotableFieldNamesH h (G.S1 (G.MetaSel ('Just name) u v w) (G.Rec0 (h String bean))) where
  gDemoteFieldNamesH f =
    G.M1 (G.K1 (f (symbolVal (Proxy @name))))

-- | Modify the outermost phase wrapping each component, while having access to
-- the field name of the component.
--
-- A typical usage is modifying a \"parsing the configuration\" phase so that
-- each component looks into a different section of the global configuration
-- field.
mapPhaseWithFieldNames ::
  forall (f :: Type -> Type) (f' :: Type -> Type) (g :: Type -> Type) (m :: Type -> Type) env_.
  ( Phased env_,
    DemotableFieldNames env_,
    Typeable f,
    Typeable f',
    Typeable g,
    Typeable m
  ) =>
  -- | Transform to be applied to each field. Has access to the field name.
  (forall x. Typeable x => String -> f x -> f' x) ->
  env_ (Compose f g) m ->
  env_ (Compose f' g) m
-- f' first to help annotate the *target* of the transform?
mapPhaseWithFieldNames f env =
  liftA2Phase (\(Constant name) z -> f name z) (runIdentity $ traverseH @env_ (\(Constant z) -> Identity (Compose (Constant z))) demoteFieldNames) env

-- constructing phases

-- $phasehelpers
--
-- 'bindPhase' and 'skipPhase' are small convenience functions to help build nested compositions of functors.

-- $phasehelpers2
--
-- 'fromBare' and 'toBare' are an alternative method to build nested compositions of functors, which relies on "coerce".

-- | Use the result of the previous phase to build the next one.
--
-- Can be useful infix.
--
-- >>> :{
--  type Phases = IO `Compose` IO `Compose` Identity
--  phased :: Phases Int
--  phased =
--      pure 1 `bindPhase` \i1 ->
--      pure 2 `bindPhase` \i2 ->
--      Identity (i1 + i2)
-- :}
bindPhase :: forall f g a b. Functor f => f a -> (a -> g b) -> Compose f g b
-- f as first type parameter to help annotate the current phase
bindPhase f k = Compose (f <&> k)

-- | Don't do anything for the current phase, just wrap the next one.
--
-- >>> :{
--  type Phases = IO `Compose` IO `Compose` Identity
--  phased :: Phases Int
--  phased =
--      skipPhase $
--      skipPhase $
--      Identity 1
-- :}
skipPhase :: forall f g a. Applicative f => g a -> Compose f g a
-- f as first type parameter to help annotate the current phase
skipPhase g = Compose (pure g)

-- | This type family clears newtypes like 'Compose', 'Identity' and 'Constant' from a composite type,
-- leaving you with a newtypeless nested type as result.
--
-- The idea is that it might be easier to construct values of the \"bare\" version of a composite type,
-- and later coerce them to the newtyped version using 'fromBare'.
--
-- This is mainly intended for defining the nested 'Applicative' \"phases\" of components that live in a 'Phased'
-- environment. It's an alternative to functions like `Dep.Env.bindPhase' and 'Dep.Env.skipPhase'.
type Bare :: Type -> Type
type family Bare x where
  Bare (Compose outer inner x) = Bare (outer (Bare (inner x)))
  Bare (Identity x) = x
  Bare (Const x k) = x
  Bare (Constant x k) = x
  Bare other = other

-- | Convert a value from its bare version to the newtyped one, usually as a step
-- towards inserting it into a 'Phased' environment.
--
-- >>> :{
-- type Phases = IO `Compose` IO `Compose` IO
-- wrapped :: Phases Int = fromBare $ pure $ pure $ pure 3
-- :}
--
-- >>> :{
-- type Phases = Constructor Int
-- wrapped :: Phases Int
-- wrapped = fromBare $ succ
-- :}
--
-- >>> :{
-- type Phases = IO `Compose` Constructor Int
-- wrapped :: Phases Int
-- wrapped = fromBare $ pure $ succ
-- :}
fromBare :: Coercible phases (Bare phases) => Bare phases -> phases
fromBare = coerce

-- | Convert from the newtyped value to the bare one. 'fromBare' tends to be more useful.
toBare :: Coercible phases (Bare phases) => phases -> Bare phases
toBare = coerce

-- | A phase with the effect of \"constructing each component by reading its
-- dependencies from a completed environment\". It should be the final phase.
--
-- The 'Constructor' phase for an environment will typically be parameterized
-- with the environment itself.
type Constructor (env :: Type) = ((->) env) `Compose` Identity

-- | Turn an environment-consuming function into a 'Constructor' that can be slotted
-- into some field of a 'Phased' environment.
constructor :: forall r_ m env. (env -> r_ m) -> Constructor env (r_ m)
-- same order of type parameters as Has
constructor = coerce

-- | A generalization of 'Constructor' which produces, in addition to the result
-- value, a value @w@ which is then aggregated across all components and fed
-- back along with the completed environment.
--
-- Like 'Constructor', 'AccumConstructor' should be the final phase.
type AccumConstructor (w :: Type) (env :: Type) = (->) (w, env) `Compose` (,) w `Compose` Identity

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
fixEnv env = fix (pullPhase env)

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
--
-- __/LAZY PATTERN MATCH REQUIRED!/__ Any constructor that matches on the
-- (accumulator, environment) tuple needs to use a lazy pattern match like
-- @~(w,env)@. Otherwise 'fixEnvAccum' enters an infinite loop! Such are the
-- dangers of knot-tying.
fixEnvAccum ::
  (Phased env_, Typeable env_, Typeable m, Monoid w, Typeable w) =>
  -- | Environment where each field is wrapped in an 'AccumConstructor'
  env_ (AccumConstructor w (env_ Identity m)) m ->
  -- | Fully constructed accumulator and environment, ready for use.
  (w, env_ Identity m)
fixEnvAccum env =
  let f = pullPhase <$> pullPhase env
   in fix f

-- | An inductively constructed environment with anonymous fields.
--
-- Can be useful for simple tests. Also for converting `Has`-based
-- components into functions that take their dependencies as separate
-- positional parameters.
--
-- > makeController :: (Monad m, Has Logger m env, Has Repository m env) => env -> Controller m
-- > makeController = undefined
-- > makeControllerPositional :: Monad m => Logger m -> Repository m -> Controller m
-- > makeControllerPositional a b = makeController $ addDep @Logger a $ addDep @Repository b $ emptyEnv
-- > makeController' :: (Monad m, Has Logger m env, Has Repository m env) => env -> Controller m
-- > makeController' env = makeControllerPositional (dep env) (dep env)
data InductiveEnv (rs :: [(Type -> Type) -> Type]) (h :: Type -> Type) (m :: Type -> Type) where
  AddDep :: forall r_ m rs h. Typeable r_ => h (r_ m) -> InductiveEnv rs h m -> InductiveEnv (r_ : rs) h m
  EmptyEnv :: forall m h. InductiveEnv '[] h m

-- | Unlike the 'AddDep' constructor, this sets @h@ to 'Identity'.
addDep :: forall r_ m rs. Typeable r_ => r_ m -> InductiveEnv rs Identity m -> InductiveEnv (r_ : rs) Identity m
addDep = AddDep @r_ @m @rs . Identity

-- | Unlike the 'EmptyEnv' constructor, this sets @h@ to 'Identity'.
emptyEnv :: forall m. InductiveEnv '[] Identity m
emptyEnv = EmptyEnv @m @Identity

instance Phased (InductiveEnv rs) where
  traverseH t EmptyEnv = pure EmptyEnv
  traverseH t (AddDep hx rest) =
    let headF = t hx
        restF = traverseH t rest
     in AddDep <$> headF <*> restF
  liftA2H t EmptyEnv EmptyEnv = EmptyEnv
  liftA2H t (AddDep ax arest) (AddDep hx hrest) =
    AddDep (t ax hx) (liftA2H t arest hrest)

-- | Works by searching on the list of types.
instance InductiveEnvFind r_ m rs => Has r_ m (InductiveEnv rs Identity m) where
  dep = inductiveEnvDep

class InductiveEnvFind r_ m rs where
  inductiveEnvDep :: InductiveEnv rs Identity m -> r_ m

instance
  TypeError
    ( Text "The component "
        :<>: ShowType r_
        :<>: Text " could not be found in environment."
    ) =>
  InductiveEnvFind r_ m '[]
  where
  inductiveEnvDep = error "never happens"

instance InductiveEnvFind' (r_ == r_') r_ m (r_' : rs) => InductiveEnvFind r_ m (r_' : rs) where
  inductiveEnvDep = inductiveEnvDep' @(r_ == r_')

class InductiveEnvFind' (matches :: Bool) r_ m rs where
  inductiveEnvDep' :: InductiveEnv rs Identity m -> r_ m

instance InductiveEnvFind' True r_ m (r_ : rs) where
  inductiveEnvDep' (AddDep (Identity r) _) = r

instance InductiveEnvFind r_ m rs => InductiveEnvFind' False r_ m (x : rs) where
  inductiveEnvDep' (AddDep _ rest) = inductiveEnvDep rest
