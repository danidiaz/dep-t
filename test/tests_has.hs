{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Main (main) where

import Control.Monad.Dep
import Control.Monad.Dep.Has
import Control.Monad.Dep.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Coerce
import Data.Kind
import Data.List (intercalate)
import Data.SOP
import GHC.Generics
import Rank2 qualified
import Rank2.TH qualified
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (log)
import Data.Functor.Identity
import GHC.Records

-- https://stackoverflow.com/questions/53498707/cant-derive-generic-for-this-type/53499091#53499091
-- There are indeed some higher kinded types for which GHC can currently derive Generic1 instances, but the feature is so limited it's hardly worth mentioning. This is mostly an artifact of taking the original implementation of Generic1 intended for * -> * (which already has serious limitations), turning on PolyKinds, and keeping whatever sticks, which is not much.
type Logger :: (Type -> Type) -> Type
newtype Logger d = Logger {log :: String -> d ()} deriving (Generic)

instance Dep Logger where
  type DefaultFieldName Logger = "logger"

data Repository d = Repository
  { select :: String -> d [Int],
    insert :: [Int] -> d ()
  }
  deriving (Generic)

instance Dep Repository where
  type DefaultFieldName Repository = "repository"

newtype Controller d = Controller {serve :: Int -> d String} deriving (Generic)

instance Dep Controller where
  type DefaultFieldName Controller = "controller"

type Env :: (Type -> Type) -> Type
data Env m = Env
  { logger :: Logger m,
    repository :: Repository m,
    controller :: Controller m
  }

instance Has Logger m (Env m)

instance Has Repository m (Env m)

instance Has Controller m (Env m)

mkController :: forall d e m. MonadDep [Has Logger, Has Repository] d e m => Controller m
mkController =
  Controller \url -> 
    useEnv \(asCall -> call) -> do
      call log "I'm going to insert in the db!"
      call select "select * from ..."
      call insert [1, 2, 3, 4]
      return "view"

-- also toss in this helper function
-- useEnv :: forall d e m r. (LiftDep d m, MonadReader e m) => (e -> d r) -> m r
-- useEnv f = do
--   e <- ask
--   liftD (f e)

-- better than with all that liftD spam... although slightly less flexible
mkController' :: forall d e m. MonadDep [Has Logger, Has Repository] d e m => Controller m
mkController' =
  Controller \url ->
    useEnv \e -> do
      let (asCall -> call) = e
      call @Logger log "I'm going to insert in the db!"
      call @Repository select "select * from ..."
      call insert [5, 3, 43]
      return "view"

type EnvIO :: Type
data EnvIO = EnvIO
  { logger :: Logger IO,
    repository :: Repository IO
  }

instance Has Logger IO EnvIO

instance Has Repository IO EnvIO

type TestTrace = ([String], [Int])

mkFakeLogger :: MonadWriter TestTrace m => Logger m
mkFakeLogger = Logger \msg -> tell ([msg], [])

mkFakeRepository :: (MonadDep '[Has Logger] d e m, MonadWriter TestTrace m) => Repository m
mkFakeRepository =
  Repository
    { select = \_ -> do
        e <- ask
        liftD $ log (dep e) "I'm going to select an entity"
        return [],
      insert = \entity -> do
        (asCall -> call) <- ask
        liftD $ call log "I'm going to write the entity!"
        tell ([], entity)
    }

env :: Env (DepT Env (Writer TestTrace))
env =
  let logger = mkFakeLogger
      repository = mkFakeRepository
      controller = mkController
   in Env {logger, repository, controller}

--
-- to test the coercible in the definition of Has
type EnvHKD :: (Type -> Type) -> (Type -> Type) -> Type
data EnvHKD h m = EnvHKD
  { logger :: h (Logger m),
    repository :: h (Repository m),
    controller :: h (Controller m)
  }

instance Has Logger m (EnvHKD I m)

instance Has Repository m (EnvHKD I m)

instance Has Controller m (EnvHKD I m)


type EnvHKD2 :: (Type -> Type) -> (Type -> Type) -> Type
data EnvHKD2 h m = EnvHKD2
  { logger :: h (Logger m),
    repository :: h (Repository m),
    controller :: h (Controller m)
  } deriving (Generic)

deriving via (FirstFieldOfType (EnvHKD2 Identity m)) instance Has Logger m (EnvHKD2 Identity m)
deriving via (FirstFieldOfType (EnvHKD2 Identity m)) instance Has Repository m (EnvHKD2 Identity m)

findLogger2 :: EnvHKD2 Identity m -> Logger m
findLogger2 env = dep env

type EnvHKD3 :: (Type -> Type) -> (Type -> Type) -> Type
data EnvHKD3 h m = EnvHKD3
  { logger :: h (Logger m),
    repository :: h (Repository m),
    controller :: h (Controller m)
  } deriving (Generic)
deriving via (FirstFieldOfType (EnvHKD3 Identity m)) instance 
    ExistsNamedFieldOfType (r_ m) (EnvHKD3 Identity m) name u 
    => Has r_ m (EnvHKD3 Identity m)

findLogger3 :: EnvHKD3 Identity m -> Logger m
findLogger3 env = dep env

--
--
tests :: TestTree
tests =
  testGroup
    "All"
    []

main :: IO ()
main = defaultMain tests
