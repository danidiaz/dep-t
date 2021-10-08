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
--    Controller \url -> useCaller \call -> do
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
        -- * self
    ,   useCaller
    ) where

import DI
import Control.Monad.Dep.Class
import Control.Monad.Reader.Class

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

-- | Avoids repeated calls to 'liftD' when all the effects in a function come
--   from the environment.
--
--   Similar to 'useEnv', but the callback receives a \"call\" function that
--   pre-applies any record selector with the correct record extracted from the
--   environment.
useCaller :: forall d env m y . (LiftDep d m, MonadReader env m) => ((forall r_ x . Has r_ d env => (r_ d -> x) -> x) -> d y) -> m y
useCaller usesCaller = do
  (asCaller -> call) <- ask @env
  liftD (usesCaller call)

