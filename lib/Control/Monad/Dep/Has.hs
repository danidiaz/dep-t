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

-- | This module provides a general-purpose \"Has\" class favoring a style in
-- which the components of the environment come wrapped in records or newtypes,
-- instead of being bare functions.
--
-- >>> :{
--  type Logger :: (Type -> Type) -> Type
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
--  type Env :: (Type -> Type) -> Type
--  data Env m = Env
--    { logger :: Logger m,
--      repository :: Repository m,
--      controller :: Controller m
--    }
--  instance Has Logger m (Env m)
--  instance Has Repository m (Env m)
--  instance Has Controller m (Env m)
--  --
--  mkController :: MonadDep [Has Logger, Has Repository] d e m => Controller m
--  mkController =
--    Controller \url -> useCall \call -> do
--      call log "I'm going to insert in the db!"
--      call select "select * from ..."
--      call insert [1, 2, 3, 4]
--      return "view"
-- :}
--
-- The @adviseRecord@ and @deceiveRecord@ functions from the companion package
-- \"dep-t-advice\" can facilitate working with this style of components.
--
module Control.Monad.Dep.Has (
        -- * A general-purpose Has
        Has (..) 
        -- * Component defaults
    ,   Dep (..)
        -- * call helper
    ,   asCall
    ,   useCall
    ) where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Data.Coerce
import Control.Monad.Reader
import Control.Monad.Dep.Class

-- | A generic \"Has\" class. When partially applied to a parametrizable
-- record-of-functions @r_@, produces a 2-place constraint that can be later
-- used with "Control.Monad.Dep.Class".
type Has :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type -> Constraint
class Has r_ m env | env -> m where
  -- |  Given an environment @e@, produce a record-of-functions parameterized by the environment's effect monad @d@.
  --
  -- The hope is that using a selector function on the resulting record will
  -- determine its type without the need for type annotations.
  --
  -- (This will likely not play well with RecordDotSyntax. See also <https://chrisdone.com/posts/import-aliases-field-names/ this trick>.)
  dep :: env -> r_ m
  default dep :: (Dep r_, HasField (DefaultFieldName r_) env u, Coercible u (r_ m)) => env -> r_ m
  dep env = coerce . getField @(DefaultFieldName r_) $ env

asCall :: forall env m . env -> forall r_ x. Has r_ m env => (r_ m -> x) -> x
asCall env = \f -> f (dep env)

-- | Avoids repeated calls to 'liftD' when all the effects in a function come
--   from the environment.
--
--   Similar to 'useEnv', but the callback receives a \"call\" function that
--   pre-applies any record selector with the correct record extracted from the
--   environment.
useCall :: forall d env m y . (LiftDep d m, MonadReader env m) => ((forall r_ x . Has r_ d env => (r_ d -> x) -> x) -> d y) -> m y
useCall usesCall = do
  (asCall -> call) <- ask @env
  liftD (usesCall call)

-- | Parametrizable records-of-functions can be given an instance of this
-- typeclass to specify the default field name 'Has' expects for the component
-- in the environment record.
--
-- This allows defining 'Has' instances with empty bodies, thanks to
-- @DefaultSignatures@.
type Dep :: ((Type -> Type) -> Type) -> Constraint
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
-- >>> import Data.Kind
-- >>> import Control.Monad.Dep
-- >>> import GHC.Generics (Generic)
--


