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
module Control.Monad.Dep.Has.Env (
      -- * A general-purpose Has
      Has
      -- * Helpers for deriving Has
      -- ** via the default field name
    , TheDefaultFieldName (..)
      -- ** via arbitrary field name
    , TheFieldName (..)
      -- ** via Autowiring
    , FieldsFindableByType (..)
    , Autowire (..)
    , Autowireable
      -- * Managing phases
    , Phased (..)
      -- * Re-exports
    , fix
    ) where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Data.Coerce
import GHC.Generics qualified as G
import Control.Applicative
import Control.Monad.Dep.Has 
import Data.Functor.Compose
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
type Autowire :: Type -> Type
newtype Autowire env = Autowire env

-- | Constraints required when @DerivingVia@ all possible instances of 'Has' in
-- a single definition.
--
-- @wrapping@ should be @r_ m@ when the components don't come wrapped in some
-- newtype, and @somenewtype (r_ m)@ otherwise.
type Autowireable wrapping r_ m env =
         ( HasField (FindFieldByType env (r_ m)) env wrapping
         , Coercible wrapping (r_ m) )

instance (
           FieldsFindableByType (env_ m),
           Autowireable wrapping r_ m (env_ m) 
         ) 
         => Has r_ m (Autowire (env_ m)) where
   dep (Autowire env) = coerce (getField @(FindFieldByType (env_ m) (r_ m)) env)

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
    pullPhase :: (Applicative f, Applicative g) => env_ (Compose f g) m -> f (env_ g m)
    default pullPhase 
        :: ( G.Generic (env_ (Compose f g) m)
           , G.Generic (env_ g m)
           , GPullPhase f g (G.Rep (env_ (Compose f g) m)) (G.Rep (env_ g m))
           , Applicative f
           , Applicative g )
        => env_ (Compose f g) m -> f (env_ g m)
    pullPhase env = G.to <$> gPullPhase (G.from env)
    mapPhase :: (Applicative f, Applicative f') 
        => (forall x. f x -> f' x) -> env_ (Compose f g) m -> env_ (Compose f' g) m
    default mapPhase 
        :: ( G.Generic (env_ (Compose f g) m)
           , G.Generic (env_ (Compose f' g) m)
           , GMapPhase f f' (G.Rep (env_ (Compose f g) m)) (G.Rep (env_ (Compose f' g) m))
           , Applicative f
           , Applicative f')
        => (forall x. f x -> f' x) -> env_ (Compose f g) m -> env_ (Compose f' g) m
    mapPhase f env = G.to (gMapPhase f (G.from env))

class GPullPhase f g env env' | env -> env' f g where
    gPullPhase :: env x -> f (env' x)

instance (Functor f , GPullPhase f g fields fields')
    => GPullPhase f 
               g
               (G.D1 metaData (G.C1 metaCons fields)) 
               (G.D1 metaData (G.C1 metaCons fields')) where
    gPullPhase (G.M1 (G.M1 fields)) = 
        G.M1 . G.M1 <$> gPullPhase @f @g fields

instance (Applicative f,
          GPullPhase f g left left',
          GPullPhase f g right right') 
        => GPullPhase f g (left G.:*: right) (left' G.:*: right') where
     gPullPhase (left G.:*: right) = 
        let left' = gPullPhase @f @g left
            right' = gPullPhase @f @g right
         in liftA2 (G.:*:) left' right'

instance (Applicative f, Applicative g) 
    => GPullPhase f g (G.S1 metaSel (G.Rec0 (Compose f g bean))) 
                   (G.S1 metaSel (G.Rec0 (g bean))) where
     gPullPhase (G.M1 (G.K1 (Compose fgbean))) =
         G.M1 . G.K1 <$> fgbean 

-- 
--
class GMapPhase f f' env env' | env -> f, env' -> f' where
    gMapPhase :: (forall r. f r -> f' r) -> env x -> env' x

instance (Functor f , GMapPhase f f' fields fields')
    => GMapPhase f 
               f'
               (G.D1 metaData (G.C1 metaCons fields)) 
               (G.D1 metaData (G.C1 metaCons fields')) where
    gMapPhase f (G.M1 (G.M1 fields)) = 
        G.M1 (G.M1 (gMapPhase @f @f' f fields))

instance (Applicative f,
          GMapPhase f f' left left',
          GMapPhase f f' right right') 
        => GMapPhase f f' (left G.:*: right) (left' G.:*: right') where
     gMapPhase f (left G.:*: right) = 
        let left' = gMapPhase @f @f' f left
            right' = gMapPhase @f @f' f right
         in (G.:*:) left' right'

instance (Applicative f, Applicative f') 
    => GMapPhase f f' (G.S1 metaSel (G.Rec0 (Compose f g bean))) 
                      (G.S1 metaSel (G.Rec0 (Compose f' g bean))) where
     gMapPhase f (G.M1 (G.K1 (Compose fgbean))) =
         G.M1 (G.K1 (Compose (f fgbean)))


-- class Multifixable (cc_ :: (Type -> Type) -> Type) where
--     multifix 
--         :: Open cc_ -> Closed cc_
--     default multifix 
--         :: ( Generic (Open cc_)
--            , Generic (Closed cc_)
--            , GMultifixable (Closed cc_)
--                            (Rep (Open cc_))
--                            (Rep (Closed cc_))
--            )
--         => Open cc_ -> Closed cc_
--     multifix cc_ =
--         -- Dependency injection by knot-tying.
--         let result = to (gMultifix result (from cc_))  
--          in result
-- 
-- class GMultifixable final g g' where
--     gMultifix :: final -> g x -> g' x
-- 
-- 
-- instance GMultifixable final fields fields'
--     => GMultifixable final (D1 metaData (C1 metaCons fields)) 
--                              (D1 metaData (C1 metaCons fields')) where
--     gMultifix final (M1 (M1 fields)) = 
--         M1 (M1 (gMultifix final fields))
-- 
-- instance Instrumentable bean 
--     => GMultifixable final (S1 metaSel (Rec0 (final -> bean))) 
--                            (S1 metaSel (Rec0 (Identity bean))) where
--      gMultifix final (M1 (K1 beanf)) =
--         let bean' = beanf $ final
--          in M1 (K1 (Identity bean'))
-- 
-- instance (GMultifixable final left left',
--           GMultifixable final right right') 
--         => GMultifixable final (left :*: right) (left' :*: right') where
--      gMultifix final (left :*: right) = 
--         let left' = gMultifix final left
--             right' = gMultifix final right
--          in left' :*: right'
