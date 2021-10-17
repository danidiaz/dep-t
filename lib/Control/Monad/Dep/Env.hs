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
    , Phases
    , Phased (..)
      -- ** Working with field names
    , DemotableFieldNames (..)
    , mapPhaseWithFieldNames
      -- * Injecting dependencies by tying the knot
    , fixConstructors
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
type Autowireable r_ m env =
         ( HasField (FindFieldByType env (r_ m)) env (Identity (r_ m))
         -- Worth having this?
         -- , Coercible wrapping (r_ m) 
         )

instance (
           FieldsFindableByType (env_ m),
           Autowireable r_ m (env_ m) 
         ) 
         => Has r_ m (Autowired (env_ m)) where
   dep (Autowired env) = coerce (getField @(FindFieldByType (env_ m) (r_ m)) env)

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

type Phases :: [Type -> Type] -> Type -> Type 
newtype Phases ps a = Phases ((Nested ps) a)

type Nested :: [Type -> Type] -> (Type -> Type)
type family Nested ps where
    Nested '[] = Identity
    Nested (p ': ps) = p `Compose` Nested ps

type Phased :: ((Type -> Type) -> (Type -> Type) -> Type) -> Constraint
class Phased env_ where
    pullPhase :: Applicative f => env_ (Phases (f : ps)) m -> f (env_ (Phases ps) m)
    default pullPhase 
        :: ( G.Generic (env_ (Phases (f : ps)) m)
           , G.Generic (env_ (Phases      ps ) m)
           , GPullPhase f ps (G.Rep (env_ (Phases (f : ps)) m)) (G.Rep (env_ (Phases ps) m))
           , Applicative f )
        => env_ (Phases (f : ps)) m -> f (env_ (Phases ps) m)
    pullPhase env = G.to <$> gPullPhase (G.from env)
    mapPhase :: (forall x. f x -> f' x) -> env_ (Phases (f : ps)) m -> env_ (Phases (f' : ps)) m
    default mapPhase 
        :: ( G.Generic (env_ (Phases (f : ps)) m)
           , G.Generic (env_ (Phases      ps ) m)
           , GMapPhase f f' (G.Rep (env_ (Phases (f : ps)) m)) (G.Rep (env_ (Phases (f' : ps)) m))
           )
        => (forall x. f x -> f' x) -> env_ (Phases (f : ps)) m -> env_ (Phases (f' : ps)) m
    mapPhase f env = G.to (gMapPhase f (G.from env))
    liftA2Phase :: (forall x. a x -> f x -> f' x) -> env_ (Phases (a : ps)) m -> env_ (Phases (f : ps)) m -> env_ (Phases (f' : ps)) m
    default liftA2Phase 
        :: ( G.Generic (env_ (Phases (a : ps)) m)
           , G.Generic (env_ (Phases (f : ps)) m)
           , G.Generic (env_ (Phases (f' : ps)) m)
           , GLiftA2Phase a f f' (G.Rep (env_ (Phases (a : ps)) m)) (G.Rep (env_ (Phases (f : ps)) m)) (G.Rep (env_ (Phases (f' : ps)) m))
           )
        => (forall x. a x -> f x -> f' x) -> env_ (Phases (a : ps)) m -> env_ (Phases (f : ps)) m -> env_ (Phases (f' : ps)) m
    liftA2Phase f enva env = G.to (gLiftA2Phase f (G.from enva) (G.from env))


class GPullPhase f ps env env' | env -> env' f ps where
    gPullPhase :: env x -> f (env' x)

instance (Functor f , GPullPhase f ps fields fields')
    => GPullPhase f 
               ps 
               (G.D1 metaData (G.C1 metaCons fields)) 
               (G.D1 metaData (G.C1 metaCons fields')) where
    gPullPhase (G.M1 (G.M1 fields)) = 
        G.M1 . G.M1 <$> gPullPhase @f @ps fields

instance (Applicative f,
          GPullPhase f ps left left',
          GPullPhase f ps right right') 
        => GPullPhase f ps (left G.:*: right) (left' G.:*: right') where
     gPullPhase (left G.:*: right) = 
        let left' = gPullPhase @f @ps left
            right' = gPullPhase @f @ps right
         in liftA2 (G.:*:) left' right'

instance (Functor f) 
    => GPullPhase f ps (G.S1 metaSel (G.Rec0 (Phases (f : ps) bean))) 
                   (G.S1 metaSel (G.Rec0 ((Phases ps) bean))) where
     gPullPhase (G.M1 (G.K1 (Phases (Compose fgbean)))) =
         G.M1 . G.K1 . Phases <$> fgbean 

-- 
--
class GMapPhase f f' env env' | env -> f, env' -> f' where
    gMapPhase :: (forall r. f r -> f' r) -> env x -> env' x

instance (GMapPhase f f' fields fields')
    => GMapPhase f 
               f'
               (G.D1 metaData (G.C1 metaCons fields)) 
               (G.D1 metaData (G.C1 metaCons fields')) where
    gMapPhase f (G.M1 (G.M1 fields)) = 
        G.M1 (G.M1 (gMapPhase @f @f' f fields))

instance ( GMapPhase f f' left left',
           GMapPhase f f' right right') 
        => GMapPhase f f' (left G.:*: right) (left' G.:*: right') where
     gMapPhase f (left G.:*: right) = 
        let left' = gMapPhase @f @f' f left
            right' = gMapPhase @f @f' f right
         in (G.:*:) left' right'

instance  GMapPhase f f' (G.S1 metaSel (G.Rec0 (Phases (f : ps) bean))) 
                      (G.S1 metaSel (G.Rec0 (Phases (f' : ps) bean))) where
     gMapPhase f (G.M1 (G.K1 (Phases (Compose fgbean)))) =
         G.M1 (G.K1 (Phases (Compose (f fgbean))))

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

instance   GLiftA2Phase a f f' (G.S1 metaSel (G.Rec0 (Phases (a : ps) bean)))
                               (G.S1 metaSel (G.Rec0 (Phases (f : ps) bean))) 
                               (G.S1 metaSel (G.Rec0 (Phases (f' : ps) bean))) where
     gLiftA2Phase f (G.M1 (G.K1 (Phases (Compose abean)))) (G.M1 (G.K1 (Phases (Compose fgbean)))) =
         G.M1 (G.K1 (Phases (Compose (f abean fgbean))))

-- Demotable field names
type DemotableFieldNames :: ((Type -> Type) -> (Type -> Type) -> Type) -> Constraint
class DemotableFieldNames env_ where
    demoteFieldNames :: env_ (Phases (Constant String : ps)) m
    default demoteFieldNames 
        :: ( G.Generic (env_ (Phases (Constant String : ps)) m)
           , GDemotableFieldNames g (G.Rep (env_ (Phases (Constant String : ps)) m)))
           => env_ (Phases (Constant String : ps)) m
    demoteFieldNames = G.to gDemoteFieldNames

class GDemotableFieldNames ps env | env -> ps where
    gDemoteFieldNames :: env x
            
instance GDemotableFieldNames ps fields
    => GDemotableFieldNames ps (G.D1 metaData (G.C1 metaCons fields)) where
    gDemoteFieldNames = G.M1 (G.M1 gDemoteFieldNames)

instance ( GDemotableFieldNames ps left,
           GDemotableFieldNames ps right) 
        => GDemotableFieldNames ps (left G.:*: right) where
     gDemoteFieldNames = 
         gDemoteFieldNames G.:*: gDemoteFieldNames

instance KnownSymbol name => GDemotableFieldNames ps (G.S1 (G.MetaSel ('Just name) u v w) (G.Rec0 (Phases (Constant String : ps) bean))) where
     gDemoteFieldNames = 
         G.M1 (G.K1 (Phases (Compose (Constant (symbolVal (Proxy @name))))))

mapPhaseWithFieldNames :: (Phased env_, DemotableFieldNames env_, Applicative f, Applicative f') 
    => (forall x. String -> f x -> f' x) -> env_ (Phases (f : ps)) m -> env_ (Phases (f' : ps)) m
mapPhaseWithFieldNames  f env =
    liftA2Phase (\(Constant name) z -> f name z) demoteFieldNames env

fixConstructors :: Phased env_ => env_ (Phases '[(->) (env_ Identity m)]) m -> env_ Identity m
fixConstructors env = fix (pullPhase env)

