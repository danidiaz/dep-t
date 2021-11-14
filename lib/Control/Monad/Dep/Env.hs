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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

-- | This module provides helpers for building dependency injection
-- environments composed of records.
--
-- It's not necessary when defining the record components themselves, in that
-- case 'Control.Monad.Dep.Has' should suffice.
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
-- -- deriving anyclass (FieldsFindableByType, DemotableFieldNames, Phased)
-- -- deriving via Autowired (EnvHKD Identity m) instance Autowireable r_ m (EnvHKD Identity m) => Has r_ m (EnvHKD Identity m)
-- :}
--
-- The module also provides a monad transformer-less way of performing dependency
-- injection, by means of 'fixEnv'.
--
module Control.Monad.Dep.Env (
      -- * A general-purpose Has
      Has
      -- * Helpers for deriving Has
      -- ** via the default field name
    , TheDefaultFieldName (..)
      -- ** via arbitrary field name
    , TheFieldName (..)
      -- ** via autowiring
    , FieldsFindableByType (..)
    , Autowired (..)
    , Autowireable
      -- * Managing phases
    , Phased (..)
    , pullPhase
    , mapPhase
    , liftA2Phase
      -- ** Working with field names
    , DemotableFieldNames (..)
    , demoteFieldNames
    , mapPhaseWithFieldNames
      -- ** Constructing phases
      -- $phasehelpers
    , bindPhase
    , skipPhase  
      -- * Injecting dependencies by tying the knot
    , fixEnv
    , Constructor
    , constructor
      -- * Inductive environment with anonymous fields
    , InductiveEnv (..)
    , addDep
    , emptyEnv
    -- * Re-exports
    , Identity (..)
    , Constant (..)
    , Compose (..)
    ) where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Data.Coerce
import GHC.Generics qualified as G
import Control.Applicative
import Control.Monad.Dep.Has 
import Data.Proxy
import Data.Functor ((<&>), ($>))
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Function (fix)
import Data.String
import Data.Type.Equality (type (==))
import Data.Typeable

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
-- >>> import Data.Kind
-- >>> import Control.Monad.Dep.Has
-- >>> import Control.Monad.Dep.Env
-- >>> import GHC.Generics (Generic)
--


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

instance (Dep r_, HasField (DefaultFieldName r_) (env_ m) u, Coercible u (r_ m)) 
         => Has r_ m (TheDefaultFieldName (env_ m)) where
   dep (TheDefaultFieldName env) = coerce . getField @(DefaultFieldName r_) $ env

-- | Helper for @DerivingVia@ 'HasField' instances.
--
-- The field name is specified as a 'Symbol'.
type TheFieldName :: Symbol -> Type -> Type
newtype TheFieldName (name :: Symbol) (env :: Type) = TheFieldName env

instance (HasField name (env_ m) u, Coercible u (r_ m)) 
         => Has r_ m (TheFieldName name (env_ m)) where
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

instance (
           FieldsFindableByType (env_ m),
           HasField (FindFieldByType (env_ m) (r_ m)) (env_ m) u,
           Coercible u (r_ m) 
         ) 
         => Has r_ m (Autowired (env_ m)) where
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
        TypeError (
                 Text "The component " 
            :<>: ShowType r 
            :<>: Text " could not be found in environment.")
    IfMissing _ (Just name) = name

-- The k -> Type alwasy trips me up
type GFindFieldByType :: (k -> Type) -> Type -> Maybe Symbol
type family GFindFieldByType r x where
    GFindFieldByType (left G.:*: right)                                          r = 
        WithLeftResult_ (GFindFieldByType left r) right r
    GFindFieldByType (G.S1 (G.MetaSel ('Just name) _ _ _) (G.Rec0 r))            r = Just name
    -- Here we are saying "any wrapper whatsoever over r". Too general?
    -- If the wrapper is not coercible to the underlying r, we'll fail later.
    GFindFieldByType (G.S1 (G.MetaSel ('Just name) _ _ _) (G.Rec0 (_ r)))        r = Just name
    GFindFieldByType _                                                           _ = Nothing

type WithLeftResult_ :: Maybe Symbol -> (k -> Type) -> Type -> Maybe Symbol 
type family WithLeftResult_ leftResult right r where
    WithLeftResult_ ('Just ls) right r = 'Just ls
    WithLeftResult_ Nothing    right r = GFindFieldByType right r

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
    traverseH 
        :: forall (h :: Type -> Type) (f :: Type -> Type) (g :: Type -> Type) (m :: Type -> Type). 
        ( Applicative f 
        , Typeable f
        , Typeable g
        , Typeable h
        , Typeable m
        )
        => (forall x . h x -> f (g x)) -> env_ h m -> f (env_ g m)
    default traverseH 
        :: forall (h :: Type -> Type) (f :: Type -> Type) (g :: Type -> Type) (m :: Type -> Type). 
        ( Applicative f 
        , Typeable f
        , Typeable g
        , Typeable h
        , Typeable m
        , G.Generic (env_ h m)
        , G.Generic (env_ g m)
        , GTraverseH h g (G.Rep (env_ h m)) (G.Rep (env_ g m))
        )
        => (forall x . h x -> f (g x)) -> env_ h m -> f (env_ g m)
    traverseH t env = G.to <$> gTraverseH t (G.from env)
    -- | Used to implement 'liftA2Phase', typically you should use that function instead.
    liftA2H
        :: forall (a :: Type -> Type) (f :: Type -> Type) (f' :: Type -> Type) m .
        ( Typeable a
        , Typeable f
        , Typeable f'
        , Typeable m
        )
        => (forall x. a x -> f x -> f' x) -> env_ a m -> env_ f m -> env_ f' m
    default liftA2H
        :: forall (a :: Type -> Type) (f :: Type -> Type) (f' :: Type -> Type) m .
        ( Typeable a
        , Typeable f
        , Typeable f'
        , Typeable m
        , G.Generic (env_ a m)
        , G.Generic (env_ f m)
        , G.Generic (env_ f' m)
        , GLiftA2Phase a f f' (G.Rep (env_ a m)) (G.Rep (env_ f m)) (G.Rep (env_ f' m))
        )
        => (forall x. a x -> f x -> f' x) -> env_ a m -> env_ f m -> env_ f' m
    liftA2H f enva env = G.to (gLiftA2Phase f (G.from enva) (G.from env))

-- | Take the outermost phase wrapping each component and \"pull it outwards\",
-- aggregating the phase's applicative effects.
pullPhase :: forall env_ f (g :: Type -> Type) m . (Applicative f, Phased env_, Typeable f, Typeable g, Typeable m) => env_ (Compose f g) m -> f (env_ g m)
-- f first to help annotate the phase
pullPhase = traverseH @env_ getCompose

-- | Modify the outermost phase wrapping each component.
mapPhase :: forall env_ f' f (g :: Type -> Type) m . (Phased env_ , Typeable f', Typeable f, Typeable g, Typeable m) => (forall x. f x -> f' x) -> env_ (Compose f g) m -> env_ (Compose f' g) m
-- f' first to help annotate the *target* of the transform?
mapPhase f env = runIdentity $ traverseH @env_ (\(Compose fg) -> Identity (Compose (f fg))) env

-- | Combine two environments with a function that works on their outermost phases.
liftA2Phase :: forall env_ f' (a :: Type -> Type) (f :: Type -> Type) (g :: Type -> Type) m . (Phased env_, Typeable f', Typeable a, Typeable f, Typeable g, Typeable m) => (forall x. a x -> f x -> f' x) -> env_ (Compose a g) m -> env_ (Compose f g) m -> env_ (Compose f' g) m
-- f' first to help annotate the *target* of the transform?
liftA2Phase f = liftA2H @env_ (\(Compose fa) (Compose fg) -> Compose (f fa fg))

class GTraverseH h g env env' | env -> h, env' -> g where
    gTraverseH :: Applicative f => (forall x . h x -> f (g x)) -> env x -> f (env' x)

instance (GTraverseH h g fields fields')
    => GTraverseH h 
               g
               (G.D1 metaData (G.C1 metaCons fields)) 
               (G.D1 metaData (G.C1 metaCons fields')) where
    gTraverseH t (G.M1 (G.M1 fields)) = 
        G.M1 . G.M1 <$> gTraverseH @h @g t fields

instance (GTraverseH h g left left',
          GTraverseH h g right right') 
        => GTraverseH h g (left G.:*: right) (left' G.:*: right') where
     gTraverseH t (left G.:*: right) = 
        let left' = gTraverseH @h @g t left
            right' = gTraverseH @h @g t right
         in liftA2 (G.:*:) left' right'

instance GTraverseH h g (G.S1 metaSel (G.Rec0 (h bean))) 
                   (G.S1 metaSel (G.Rec0 (g bean))) where
     gTraverseH t (G.M1 (G.K1 (hbean))) =
         G.M1 . G.K1 <$> t hbean 
--
--
class GLiftA2Phase a f f' enva env env' | enva -> a, env -> f, env' -> f' where
    gLiftA2Phase :: (forall r. a r -> f r -> f' r) -> enva x -> env x -> env' x

instance GLiftA2Phase a f f' fieldsa fields fields'
    => GLiftA2Phase 
               a
               f 
               f'
               (G.D1 metaData (G.C1 metaCons fieldsa)) 
               (G.D1 metaData (G.C1 metaCons fields)) 
               (G.D1 metaData (G.C1 metaCons fields')) where
    gLiftA2Phase f (G.M1 (G.M1 fieldsa)) (G.M1 (G.M1 fields)) = 
        G.M1 (G.M1 (gLiftA2Phase @a @f @f' f fieldsa fields))

instance ( GLiftA2Phase a f f' lefta left left',
           GLiftA2Phase a f f' righta right right'
         ) 
         => GLiftA2Phase a f f' (lefta G.:*: righta) (left G.:*: right) (left' G.:*: right') where
     gLiftA2Phase f (lefta G.:*: righta) (left G.:*: right) = 
        let left' = gLiftA2Phase @a @f @f' f lefta left
            right' = gLiftA2Phase @a @f @f' f righta right
         in (G.:*:) left' right'

instance   GLiftA2Phase a f f' (G.S1 metaSel (G.Rec0 (a bean)))
                               (G.S1 metaSel (G.Rec0 (f bean))) 
                               (G.S1 metaSel (G.Rec0 (f' bean))) where
     gLiftA2Phase f (G.M1 (G.K1 abean)) (G.M1 (G.K1 fgbean)) =
         G.M1 (G.K1 (f abean fgbean))

-- | Class of 2-parameter environments for which it's possible to obtain the
-- names of each field as values.
type DemotableFieldNames :: ((Type -> Type) -> (Type -> Type) -> Type) -> Constraint
class DemotableFieldNames env_ where
    demoteFieldNamesH :: (forall x. String -> h String x) -> env_ (h String) m
    default demoteFieldNamesH 
        :: ( G.Generic (env_ (h String) m)
           , GDemotableFieldNamesH h (G.Rep (env_ (h String) m)))
           => (forall x. String -> h String x) 
           -> env_ (h String) m
    demoteFieldNamesH f = G.to (gDemoteFieldNamesH f)

-- | Bring down the field names of the environment to the term level and store
-- them in the accumulator of "Data.Functor.Constant".
demoteFieldNames :: forall env_ m . DemotableFieldNames env_ => env_ (Constant String) m
demoteFieldNames = demoteFieldNamesH Constant

class GDemotableFieldNamesH h env | env -> h where
    gDemoteFieldNamesH :: (forall x. String -> h String x) -> env x
            
instance GDemotableFieldNamesH h fields
    => GDemotableFieldNamesH h (G.D1 metaData (G.C1 metaCons fields)) where
    gDemoteFieldNamesH f = G.M1 (G.M1 (gDemoteFieldNamesH f))

instance ( GDemotableFieldNamesH h left,
           GDemotableFieldNamesH h right) 
        => GDemotableFieldNamesH h (left G.:*: right) where
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
    forall env_ (f :: Type -> Type) (f' :: Type -> Type) (g :: Type -> Type) m. 
    ( Phased env_ 
    , DemotableFieldNames env_
    , Typeable f
    , Typeable f'
    , Typeable g 
    , Typeable m ) 
    => (forall x. String -> f x -> f' x) -> env_ (Compose f g) m -> env_ (Compose f' g) m
-- f' first to help annotate the *target* of the transform?
mapPhaseWithFieldNames  f env =
    liftA2Phase @env_ (\(Constant name) z -> f name z) (runIdentity $ traverseH @env_ (\(Constant z) -> Identity (Compose (Constant z))) demoteFieldNames) env


-- constructing phases

-- $phasehelpers
--
-- Small convenience functions to help build nested compositions of functors.
--

-- | Use the result of the previous phase to build the next one.
--
-- Can be useful infix.
bindPhase :: forall f g a b . Functor f => f a -> (a -> g b) -> Compose f g b 
-- f as first type parameter to help annotate the current phase
bindPhase f k = Compose (f <&> k)

-- | Don't do anything for the current phase, just wrap the next one.
skipPhase :: forall f g a . Applicative f => g a -> Compose f g a 
-- f as first type parameter to help annotate the current phase
skipPhase g = Compose (pure g)

-- | A phase with the effect of \"constructing each component by reading its
-- dependencies from a completed environment\". 
--
-- The 'Constructor' phase for an environment will typically be parameterized
-- with the environment itself.
type Constructor (env_ :: (Type -> Type) -> (Type -> Type) -> Type) (m :: Type -> Type) = ((->) (env_ Identity m)) `Compose` Identity


-- | Turn an environment-consuming function into a 'Constructor' that can be slotted 
-- into some field of a 'Phased' environment.
constructor :: forall r_ env_ m . (env_ Identity m -> r_ m) -> Constructor env_ m (r_ m)
-- same order of type parameters as Has
constructor = coerce

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
-- Think of it as a version of "Data.Function.fix" that, instead of \"tying\" a single
-- function, ties a whole record of them.
--
-- The @env_ (Constructor env_ m) m@ parameter might be the result of peeling
-- away successive layers of applicative functor composition using 'pullPhase',
-- until only the wiring phase remains.
fixEnv :: (Phased env_, Typeable env_, Typeable m) => env_ (Constructor env_ m) m -> env_ Identity m
fixEnv env = fix (pullPhase env)

-- | An inductively constructed environment with anonymous fields.
--
-- Can be useful for simple tests, and also for converting `Has`-based
-- components into functions that take their dependencies as separate
-- positional parameters.
--
-- > makeController :: (Monad m, Has Logger m env, Has Repository m env) => env -> Controller m
-- > makeController = undefined
-- > makeControllerPositional :: Monad m => Logger m -> Repository m -> Controller m
-- > makeControllerPositional a b = makeController $ addDep @Logger a $ addDep @Repository b $ emptyEnv
-- 
--
--
data InductiveEnv (rs :: [(Type -> Type) -> Type]) (h :: Type -> Type) (m :: Type -> Type) where
    AddDep :: forall r_ m rs h . h (r_ m) -> InductiveEnv rs h m -> InductiveEnv (r_ : rs) h m
    EmptyEnv :: forall m h . InductiveEnv '[] h m

-- | Unlike the 'AddDep' constructor, this sets @h@ to 'Identity'.
addDep :: forall r_ m rs . r_ m -> InductiveEnv rs Identity m -> InductiveEnv (r_ : rs) Identity m
addDep = AddDep @r_ @m @rs . Identity

-- | Unlike the 'EmptyEnv' constructor, this sets @h@ to 'Identity'.
emptyEnv :: forall m . InductiveEnv '[] Identity m
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

instance  TypeError (
                 Text "The component " 
            :<>: ShowType r_ 
            :<>: Text " could not be found in environment.") => InductiveEnvFind r_ m '[] where
    inductiveEnvDep = error "never happens"

instance InductiveEnvFind' (r_ == r_') r_ m (r_' : rs) => InductiveEnvFind r_ m (r_' : rs) where
    inductiveEnvDep = inductiveEnvDep' @(r_ == r_')

class InductiveEnvFind' (matches :: Bool) r_ m rs where
    inductiveEnvDep' :: InductiveEnv rs Identity m -> r_ m

instance InductiveEnvFind' True r_ m (r_ : rs) where
    inductiveEnvDep' (AddDep (Identity r) _) = r

instance InductiveEnvFind r_ m rs => InductiveEnvFind' False r_ m (x : rs) where
    inductiveEnvDep' (AddDep _ rest) = inductiveEnvDep rest


