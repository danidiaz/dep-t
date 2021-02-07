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

-- | This module provies a generic \"Has\" class which favors a style where the
-- components of the environment come wrapped in records or newtypes, instead
-- of being bare functions.
--
-- >>> :{
--  type Logger :: (Type -> Type) -> Type
--  newtype Logger d = Logger {log :: String -> d ()} deriving Generic
--  instance Dep Logger where
--    type DefaultFieldName Logger = "logger"
--  data Repository d = Repository
--    { select :: String -> d [Int],
--      insert :: [Int] -> d ()
--    } deriving Generic
--  instance Dep Repository where
--    type DefaultFieldName Repository = "repository"
--  newtype Controller d = Controller {serve :: Int -> d String} deriving Generic
--  instance Dep Controller where
--    type DefaultFieldName Controller = "controller"
--  type Env :: (Type -> Type) -> Type
--  data Env m = Env
--    { logger :: Logger m,
--      repository :: Repository m,
--      controller :: Controller m
--    }
--  instance Has Logger m (Env m)
--  instance Has Repository m (Env m)
--  instance Has Controller m (Env m)
--  mkController :: forall d e m. MonadDep [Has Logger, Has Repository] d e m => Controller m
--  mkController =
--    Controller \url -> do
--      e <- ask
--      liftD $ log (dep e) "I'm going to insert in the db!"
--      -- liftD $ (dep e).log "I'm going to insert in the db!" -- Once RecordDotSyntax arrives...
--      liftD $ select (dep e) "select * from ..."
--      liftD $ insert (dep e) [1, 2, 3, 4]
--      return "view"
-- :}
--
-- The @adviseRecord@ and @deceiveRecord@ functions from the companion package
-- \"dep-t-advice\" provide useful functionality for this style of components.
--
module Control.Monad.Dep.Has (
        -- A generic \"Has\" typeclass.
        Has (..), 
        -- Component defaults.
        Dep (..)
    ) where

import Data.Kind
import GHC.Records
import GHC.TypeLits
import Data.Coerce

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
-- >>> :set -XRankNTypes
-- >>> :set -XBlockArguments
-- >>> :set -XFlexibleInstances
-- >>> :set -XTypeFamilies
-- >>> :set -XDeriveGeneric
-- >>> import Control.Monad.Dep
-- >>> import Rank2 qualified
-- >>> import Rank2.TH qualified
-- >>> import GHC.Generics (Generic)
--


-- | A generic \"Has\" class. When partially applied to a parametrizable
-- record-of-functions @r_@, produces a 2-place constraint that can be later
-- used with "Control.Monad.Dep.Class".
type Has :: ((Type -> Type) -> Type) -> (Type -> Type) -> Type -> Constraint
class Has r_ d e | e -> d where
  -- |  Given an environment @e@, produce a record-of-functions parameterized by the environment's effect monad @d@.
  dep :: e -> r_ d
  default dep :: (Dep r_, HasField (DefaultFieldName r_) e u, Coercible u (r_ d)) => e -> r_ d
  dep e = coerce . getField @(DefaultFieldName r_) $ e

-- | Parametrizable records-of-functions can implement this instance to specify
-- the default field name 'Has' expects for the component in the environment
-- record.
--
-- This allows defining 'Has' instances with empty bodies, thanks to
-- @DefaultSignatures@.
type Dep :: ((Type -> Type) -> Type) -> Constraint
class Dep r_ where
  -- The Char kind would be useful here, to lowercase the first letter of the
  -- k type and use it as the default preferred field name.
  type DefaultFieldName r_ :: Symbol

