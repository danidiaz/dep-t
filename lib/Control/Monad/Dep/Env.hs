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

-- | This module provides helpers for defining dependency injection
-- environments composed of records.
--
-- It's not necessary when defining the record components themselves, in that
-- case 'Control.Monad.Dep.Has' should suffice.
module Control.Monad.Dep.Env (
      -- * A general-purpose Has
      Has
      -- * Helpers for deriving Has
      -- ** via the default field name
    , TheDefaultFieldName (..)
      -- ** via arbitrary field name
    , TheFieldName (..)
      -- ** via Autowiring
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
      -- * Injecting dependencies by tying the knot
    , Constructor
    , constructor
    , fixEnv
    ) where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Data.Coerce
import GHC.Generics qualified as G
import Control.Applicative
import Control.Monad.Dep.Has 
import Data.Proxy
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Function (fix)
import Data.String
-- import Control.Monad.Reader
-- import Control.Monad.Dep.Class

-- via the default field name

-- | Helper for @DerivingVia@ 'HasField' instances.
--
-- It expects the component to have as field name the default fieldname
-- specified by 'Dep'.
--
-- This is the same behavior as the @DefaultSignatures@ implementation for
-- 'Has', so maybe it doesn't make much sense to use it, except for
-- explicitness.
newtype TheDefaultFieldName env = TheDefaultFieldName env

instance (Dep r_, HasField (DefaultFieldName r_) (env_ m) u, Coercible u (r_ m)) 
         => Has r_ m (TheDefaultFieldName (env_ m)) where
   dep (TheDefaultFieldName env) = coerce . getField @(DefaultFieldName r_) $ env

-- | Helper for @DerivingVia@ 'HasField' instances.
--
-- The field name is specified as a 'Symbol'.
type TheFieldName :: Symbol -> Type -> Type
newtype TheFieldName name env = TheFieldName env

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
newtype Autowired env = Autowired env

-- | Constraints required when @DerivingVia@ all possible instances of 'Has' in
-- a single definition.
--
-- @wrapping@ should be @r_ m@ when the components don't come wrapped in some
-- newtype, and @somenewtype (r_ m)@ otherwise.
type Autowireable r_ m env = HasField (FindFieldByType env (r_ m)) env (Identity (r_ m))

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
            :<>: Text " could not be found in record.")
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

type Phased :: ((Type -> Type) -> (Type -> Type) -> Type) -> Constraint
class Phased env_ where
    traverseH :: 
        Applicative f 
        => (forall x . h x -> f (g x)) -> env_ h m -> f (env_ g m)
    default traverseH 
        :: ( G.Generic (env_ h m)
           , G.Generic (env_ g m)
           , GTraverseH h g (G.Rep (env_ h m)) (G.Rep (env_ g m))
           , Applicative f )
        => (forall x . h x -> f (g x)) -> env_ h m -> f (env_ g m)
    traverseH t env = G.to <$> gTraverseH t (G.from env)
    liftA2H ::  (forall x. a x -> f x -> f' x) -> env_ a m -> env_ f m -> env_ f' m
    default liftA2H
        :: ( G.Generic (env_ a m)
           , G.Generic (env_ f m)
           , G.Generic (env_ f' m)
           , GLiftA2Phase a f f' (G.Rep (env_ a m)) (G.Rep (env_ f m)) (G.Rep (env_ f' m))
           )
        => (forall x. a x -> f x -> f' x) -> env_ a m -> env_ f m -> env_ f' m
    liftA2H f enva env = G.to (gLiftA2Phase f (G.from enva) (G.from env))

pullPhase :: (Applicative f, Phased env_) => env_ (Compose f g) m -> f (env_ g m)
pullPhase = traverseH getCompose

mapPhase :: Phased env_ => (forall x. f x -> f' x) -> env_ (Compose f g) m -> env_ (Compose f' g) m
mapPhase f env = runIdentity $ traverseH (\(Compose fg) -> Identity (Compose (f fg))) env

liftA2Phase :: Phased env_ => (forall x. a x -> f x -> f' x) -> env_ (Compose a g) m -> env_ (Compose f g) m -> env_ (Compose f' g) m
liftA2Phase f = liftA2H (\(Compose fa) (Compose fg) -> Compose (f fa fg))

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

-- Demotable field names
type DemotableFieldNames :: ((Type -> Type) -> (Type -> Type) -> Type) -> Constraint
class DemotableFieldNames env_ where
    demoteFieldNamesH :: (forall x. String -> h String x) -> env_ (h String) m
    default demoteFieldNamesH 
        :: ( G.Generic (env_ (h String) m)
           , GDemotableFieldNamesH h (G.Rep (env_ (h String) m)))
           => (forall x. String -> h String x) 
           -> env_ (h String) m
    demoteFieldNamesH f = G.to (gDemoteFieldNamesH f)

demoteFieldNames :: DemotableFieldNames env_ => env_ (Constant String) m
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

mapPhaseWithFieldNames :: (Phased env_, DemotableFieldNames env_) 
    => (forall x. String -> f x -> f' x) -> env_ (Compose f g) m -> env_ (Compose f' g) m
mapPhaseWithFieldNames  f env =
    liftA2Phase (\(Constant name) z -> f name z) (runIdentity $ traverseH (\(Constant z) -> Identity (Compose (Constant z))) demoteFieldNames) env

type Constructor env_ m = ((->) (env_ Identity m)) `Compose` Identity

constructor :: forall env_ m r_ . (env_ Identity m -> r_ m) -> Constructor env_ m (r_ m)
constructor = coerce

fixEnv :: Phased env_ => env_ (Constructor env_ m) m -> env_ Identity m
fixEnv env = fix (pullPhase env)

--
--
data InductiveEnv rs h m where
    AddDep :: h (r_ m) -> InductiveEnv rs h m -> InductiveEnv (r_ : rs) h m
    EmptyEnv :: InductiveEnv '[] h m

instance Phased (InductiveEnv rs) where
    traverseH t EmptyEnv = pure EmptyEnv
    traverseH t (AddDep hx rest) = 
        let headF = t hx
            restF = traverseH t rest
         in AddDep <$> headF <*> restF
    liftA2H t EmptyEnv EmptyEnv = EmptyEnv
    liftA2H t (AddDep ax arest) (AddDep hx hrest) = 
        AddDep (t ax hx) (liftA2H t arest hrest)
 

