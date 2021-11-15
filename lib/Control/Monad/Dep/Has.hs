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

-- | This module provides a general-purpose 'Has' class favoring a style in
-- which the components of the environment, instead of being bare functions,
-- are themselves records or newtypes containing functions.
--
-- In this style, the functions that are \"invoked\" from the environment are
-- actually record field selectors. These selectors guide the 'Has' class to
-- find the correct records in the environment.
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
--  mkController' :: (Monad m, Has Logger m env, Has Repository m env) => env -> Controller m
--  mkController' (asCall -> call) =
--    Controller \url -> do
--      call log "I'm going to insert in the db!"
--      call select "select * from ..."
--      call insert [1, 2, 3, 4]
--      return "view"
-- :}
--
--
module Control.Monad.Dep.Has (
      -- * A general-purpose Has
      Has (..) 
      -- * call helper
    , asCall
      -- * Component defaults
    , Dep (..)
    ) where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Data.Coerce
-- import Control.Monad.Reader
-- import Control.Monad.Dep.Class

-- | A generic \"Has\" class. When partially applied to a parametrizable
-- record-of-functions @r_@, produces a 2-place constraint that can used on its
-- own, or with "Control.Monad.Dep.Class".
type Has :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type -> Constraint
class Has r_ (m :: Type -> Type) (env :: Type) | env -> m where
  -- |  Given an environment @e@, produce a record-of-functions parameterized by the environment's effect monad @m@.
  --
  -- The hope is that using a selector function on the resulting record will
  -- fix the record's type without the need for type annotations.
  --
  -- (This will likely not play well with RecordDotSyntax. See also <https://chrisdone.com/posts/import-aliases-field-names/ this import alias trick for avoiding name collisions>.)
  dep :: env -> r_ m
  default dep :: (Dep r_, HasField (DefaultFieldName r_) env u, Coercible u (r_ m)) => env -> r_ m
  dep env = coerce . getField @(DefaultFieldName r_) $ env

-- | Transforms an environment with suitable 'Has' instances into a \"helper\"
--   function that looks in the environment for the arguments of other functions.
--   Typically, the \"helped\" functions will be record field selectors.
--
--   In practice, this means that you can write @call foo@ instead of @foo (dep
--   env)@.
--
--   Using 'asCall' in a view pattern avoids having to name the
--   environment.
asCall :: forall env m . env -> forall r_ x. Has r_ m env => (r_ m -> x) -> x
asCall env = \f -> f (dep env)

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

