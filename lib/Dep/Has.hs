{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module provides a general-purpose 'Has' class favoring a style in
-- which the components of the environment, instead of being bare functions,
-- are themselves records or newtypes containing functions.
--
-- In this style, the functions that are \"invoked\" from the environment are
-- actually record field selectors. These selectors guide the 'Has' class to
-- find the correct records in the environment.
--
-- >>> :{
--  newtype Logger d = Logger {log :: String -> d ()}
--  instance Dep Logger where
--    type DefaultFieldName Logger = "logger"
--  --
--  data Repository d = Repository
--    { select :: String -> d [Int],
--      insert :: [Int] -> d ()
--    }
--  instance Dep Repository where
--    type DefaultFieldName Repository = "repository"
--  --
--  newtype Controller d = Controller {serve :: Int -> d String}
--  instance Dep Controller where
--    type DefaultFieldName Controller = "controller"
--  --
--  type Deps :: (Type -> Type) -> Type
--  data Deps m = Deps
--    { logger :: Logger m,
--      repository :: Repository m,
--      controller :: Controller m
--    }
--  instance Has Logger m (Deps m)
--  instance Has Repository m (Deps m)
--  instance Has Controller m (Deps m)
--  :}
--
-- 'Has' can be used in combination with 'MonadDep', like this:
--
-- >>> :{
--  mkController :: MonadDep [Has Logger, Has Repository] d env m => Controller m
--  mkController =
--    Controller \url ->
--      useEnv \(asCall -> call) -> do
--        call log "I'm going to insert in the db!"
--        call select "select * from ..."
--        call insert [1, 2, 3, 4]
--        return "view"
-- :}
--
-- 'Has' can also be used independently of 'MonadReader' or 'MonadDep'. Here
-- for example the environment is passed as a plain function argument, and @m@
-- doesn't have any constraint other than 'Monad':
--
-- >>> :{
--  mkController' :: (Monad m, Has Logger m deps, Has Repository m deps) => deps -> Controller m
--  mkController' (asCall -> call) =
--    Controller \url -> do
--      call log "I'm going to insert in the db!"
--      call select "select * from ..."
--      call insert [1, 2, 3, 4]
--      return "view"
-- :}
module Dep.Has
  ( -- * A general-purpose Has
    Has (..),
    HasAll,

    -- * call helpers
    asCall,
    pattern Call,
    -- , pattern Dep

    -- * Component defaults
    Dep (..),
  )
where

import Data.Coerce
import Data.Kind
import GHC.Records
import GHC.TypeLits

-- import Control.Monad.Reader
-- import Control.Monad.Dep.Class

-- | A generic \"Has\" class. When partially applied to a parametrizable
-- record-of-functions @r_@, produces a 2-place constraint
-- saying that the environment @deps@ has the record @r_@ with effect monad @m@.
--
-- The constraint can be used on its own, or with "Control.Monad.Dep.Class".
type Has :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type -> Constraint
class Has (r_ :: (Type -> Type) -> Type) (m :: Type -> Type) (deps :: Type) | deps -> m where
  -- |  Given an environment @deps@, produce a record-of-functions parameterized by the environment's effect monad @m@.
  --
  -- The hope is that using a selector function on the resulting record will
  -- fix the record's type without the need for type annotations.
  --
  -- See also <https://chrisdone.com/posts/import-aliases-field-names/ this import alias trick for avoiding name collisions>.
  dep :: deps -> r_ m
  default dep :: (Dep r_, HasField (DefaultFieldName r_) deps u, Coercible u (r_ m)) => deps -> r_ m
  dep deps = coerce . getField @(DefaultFieldName r_) $ deps

-- | When partially applied to a type-level list @rs_@ of parametrizable records-of-functions,
-- produces a 2-place constraint saying that the environment @e@ has all the
-- records @rs_@ with effect monad @m@.
type HasAll :: [(Type -> Type) -> Type] -> (Type -> Type) -> Type -> Constraint
type family HasAll rs_ m e where
  HasAll '[] m e = ()
  HasAll (r_ : rs_) m e = (Has r_ m e, HasAll rs_ m e)

instance Has r_ m (r_ m, b) where
  dep (r, _) = r

instance Has r_ m (a, r_ m) where
  dep (_, r) = r

instance Has r_ m (r_ m, b, c) where
  dep (r, _, _) = r

instance Has r_ m (a, r_ m, c) where
  dep (_, r, _) = r

instance Has r_ m (a, b, r_ m) where
  dep (_, _, r) = r

instance Has r_ m (r_ m, b, c, d) where
  dep (r, _, _, _) = r

instance Has r_ m (a, r_ m, c, d) where
  dep (_, r, _, _) = r

instance Has r_ m (a, b, r_ m, d) where
  dep (_, _, r, _) = r

instance Has r_ m (a, b, c, r_ m) where
  dep (_, _, _, r) = r

-- | A record-of-functions @r_@ might play the role of a \"component\"
-- taking part in dependency injection.
--
-- Each function field is then a \"method\". And the record field selectors are functions
-- which take the component and return the method corresponding to that field.
--
-- Given a dependency injection context, 'asCall' produces a reusable helper
-- that returns the the method corresponding to a field selector, on the condition that
-- the required 'Has' instance exists for the selectors' record.
--
-- >>> :{
--  data SomeRecord m = SomeRecord { someSelector :: String -> m () }
--  data Deps m = Deps
--    { someRecord :: SomeRecord m
--    }
--  instance Has SomeRecord m (Deps m) where
--    dep (Deps {someRecord}) = someRecord
--  :}
--
--   In practice, this just means that you can write @call someSelector@ instead of
--   @someSelector (dep deps)@:
--
-- >>> :{
--    twoInvocations :: (IO (), IO ())
--    twoInvocations =
--      let deps :: Deps IO = Deps { someRecord = SomeRecord { someSelector = putStrLn } }
--          call = asCall deps
--       in (someSelector (dep deps) "foo", call someSelector "foo")
-- :}
--
--   Using 'asCall' in a view pattern avoids having to name the
--   environment:
--
--
-- >>> :{
--    functionThatCalls :: Has SomeRecord m deps => deps -> m ()
--    functionThatCalls (asCall -> call) = call someSelector "foo"
-- :}
asCall ::
  forall deps m.
  -- | Dependency injection context that contains the components.
  deps ->
  forall r_ method.
  Has r_ m deps =>
  -- | Field selector function that extracts a method from a component.
  (r_ m -> method) ->
  method
asCall deps = \f -> f (dep deps)

-- | Pattern synonym version of 'asCall'. Slightly more succinct and doesn't
-- require @-XViewPatterns@. The synonym is unidirectional: it can only be used for
-- matching.
--
-- >>> :{
--  data SomeRecord m = SomeRecord { someSelector :: String -> m () }
--  data Deps m = Deps
--    { someRecord :: SomeRecord m
--    }
--  instance Has SomeRecord m (Deps m) where
--    dep (Deps {someRecord}) = someRecord
--  functionThatCalls :: Has SomeRecord m deps => deps -> m ()
--  functionThatCalls (Call call) = call someSelector "foo"
-- :}
pattern Call ::
  forall deps m.
  ( forall r_ method.
    Has r_ m deps =>
    (r_ m -> method) ->
    method
  ) ->
  -- | The dependency injection context that we want to match.
  deps
pattern Call call <- (asCall -> call)

{-# COMPLETE Call #-}

{-# INLINEABLE Call #-}

-- The deprecated Dep typeclass causes trouble here.
-- -- | The @δ@ should be the first argument of the functions we want to invoke.
-- pattern Dep :: forall env m . (forall r_ . Has r_ m env => r_ m) -> env
-- pattern Dep δ <- ((dep :: env -> forall r_ . Has r_ m env => r_ m) -> δ)
-- {-# COMPLETE Dep #-}
-- {-# INLINABLE Dep #-}

-- | Parametrizable records-of-functions can be given an instance of this
-- typeclass to specify the default field name 'Has' expects for the component
-- in the environment record.
--
-- This allows defining 'Has' instances with empty bodies, thanks to
-- @DefaultSignatures@.
type Dep :: ((Type -> Type) -> Type) -> Constraint

{-# DEPRECATED Dep "more intrusive than useful" #-}

class Dep r_ where
  -- The Char kind would be useful here, to lowercase the first letter of the
  -- k type and use it as the default preferred field name.
  type DefaultFieldName r_ :: Symbol

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
-- >>> :set -XScopedTypeVariables
-- >>> import Data.Kind
-- >>> import Control.Monad.Dep
-- >>> import GHC.Generics (Generic)
