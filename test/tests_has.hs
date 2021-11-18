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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Dep.Has
import Dep.Env
import Control.Monad.Dep
import Control.Monad.Dep.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Coerce
import Data.Kind
import Data.List (intercalate)
import Data.SOP hiding (Compose)
import GHC.Generics
import Rank2 qualified
import Rank2.TH qualified
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (log)
import Data.Functor.Identity
import Data.Functor.Product
import GHC.TypeLits
import Barbies
import Control.Monad.Trans.Cont
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.IORef
import Data.Functor.Compose
import Control.Exception hiding (TypeError)
import System.IO
import Data.Function

-- https://stackoverflow.com/questions/53498707/cant-derive-generic-for-this-type/53499091#53499091
-- There are indeed some higher kinded types for which GHC can currently derive Generic1 instances, but the feature is so limited it's hardly worth mentioning. This is mostly an artifact of taking the original implementation of Generic1 intended for * -> * (which already has serious limitations), turning on PolyKinds, and keeping whatever sticks, which is not much.
type Logger :: (Type -> Type) -> Type
newtype Logger d = Logger {log :: String -> d ()} deriving stock Generic

instance Dep Logger where
  type DefaultFieldName Logger = "logger"

data Repository d = Repository
  { select :: String -> d [Int],
    insert :: [Int] -> d ()
  }
  deriving (Generic)

instance Dep Repository where
  type DefaultFieldName Repository = "repository"

newtype Controller d = Controller {serve :: Int -> d String} deriving stock Generic

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

makeController :: forall d e m. MonadDep [Has Logger, Has Repository] d e m => Controller m
makeController =
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
makeController' :: forall d e m. MonadDep [Has Logger, Has Repository] d e m => Controller m
makeController' =
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

makeFakeLogger :: MonadWriter TestTrace m => Logger m
makeFakeLogger = Logger \msg -> tell ([msg], [])

makeFakeRepository :: (MonadDep '[Has Logger] d e m, MonadWriter TestTrace m) => Repository m
makeFakeRepository =
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
  let logger = makeFakeLogger
      repository = makeFakeRepository
      controller = makeController
   in Env {logger, repository, controller}

--
-- to test the coercible in the definition of Has
type EnvHKD :: (Type -> Type) -> (Type -> Type) -> Type
data EnvHKD h m = EnvHKD
  { logger :: h (Logger m),
    repository :: h (Repository m),
    controller :: h (Controller m)
  }
  deriving stock Generic
  deriving anyclass (Phased)

instance Has Logger m (EnvHKD I m)
instance Has Repository m (EnvHKD I m)
instance Has Controller m (EnvHKD I m)

--
-- Test TheDefaultFieldName
type EnvHKD' :: (Type -> Type) -> (Type -> Type) -> Type
data EnvHKD' h m = EnvHKD'
  { logger :: h (Logger m),
    repository :: h (Repository m),
    controller :: h (Controller m)
  }

deriving via (TheDefaultFieldName (EnvHKD' I m)) instance Has Logger m (EnvHKD' I m)
deriving via (TheDefaultFieldName (EnvHKD' I m)) instance Has Repository m (EnvHKD' I m)
deriving via (TheDefaultFieldName (EnvHKD' I m)) instance Has Controller m (EnvHKD' I m)

--- Test isolated instance definitions for autowired
type EnvHKD2 :: (Type -> Type) -> (Type -> Type) -> Type
data EnvHKD2 h m = EnvHKD2
  { logger :: h (Logger m),
    repository :: h (Repository m),
    controller :: h (Controller m)
  } deriving stock Generic
    deriving anyclass FieldsFindableByType 

deriving via (Autowired (EnvHKD2 Identity m)) instance Has Logger m (EnvHKD2 Identity m)
deriving via (Autowired (EnvHKD2 Identity m)) instance Has Repository m (EnvHKD2 Identity m)
deriving via (Autowired (EnvHKD2 Identity m)) instance Has Controller m (EnvHKD2 Identity m)

-- findLogger2 :: EnvHKD2 Identity m -> Logger m
-- findLogger2 env = dep env

type EnvHKD3 :: (Type -> Type) -> (Type -> Type) -> Type
data EnvHKD3 h m = EnvHKD3
  { logger :: h (Logger m),
    repository :: h (Repository m),
    controller :: h (Controller m)
  } deriving stock Generic
    deriving anyclass FieldsFindableByType
    
deriving via Autowired (EnvHKD3 Identity m) instance Autowireable r_ m (EnvHKD3 Identity m) => Has r_ m (EnvHKD3 Identity m)

findLogger3 :: EnvHKD3 Identity m -> Logger m
findLogger3 env = dep env



type EnvHKD4 :: (Type -> Type) -> (Type -> Type) -> Type
data EnvHKD4 h m = EnvHKD4
  { loggerx :: h (Logger m),
    repositoryx :: h (Repository m),
    controllerx :: h (Controller m)
  } deriving (Generic)
    
type Correspondence4 :: Type -> Symbol
type family Correspondence4 r :: Symbol where
    Correspondence4 (Logger m) = "loggerx"
    -- Correspondence4 (Identity (Logger m)) = "logger"
    Correspondence4 (Repository m) = "repositoryx"
    Correspondence4 (Controller m) = "controllerx"
    Correspondence4 _ = TypeError (Text "what")

-- non-default FieldsFindableByType instance
instance FieldsFindableByType                    (EnvHKD4 Identity m) where
    type FindFieldByType                         (EnvHKD4 Identity m) r = Correspondence4 r

deriving via Autowired (EnvHKD4 Identity m) instance Autowireable r_ m (EnvHKD4 Identity m) => Has r_ m (EnvHKD4 Identity m)

findLogger4 :: EnvHKD4 Identity m -> Logger m
findLogger4 env = dep env
findRepository4 :: EnvHKD4 Identity m -> Repository m
findRepository4 env = dep env
findController4 :: EnvHKD4 Identity m -> Controller m
findController4 env = dep env

type EnvHKD5 :: (Type -> Type) -> Type
data EnvHKD5 m = EnvHKD5
  { loggerx :: Logger m,
    repositoryx :: Repository m,
    controllerx :: Controller m
  } deriving stock Generic
    deriving anyclass FieldsFindableByType 

deriving via Autowired (EnvHKD5 m) instance Has Logger m (EnvHKD5 m)
deriving via Autowired (EnvHKD5 m) instance Has Repository m (EnvHKD5 m)
deriving via Autowired (EnvHKD5 m) instance Has Controller m (EnvHKD5 m)

findLogger5 :: EnvHKD5 m -> Logger m
findLogger5 env = dep env
findRepository5 :: EnvHKD5 m -> Repository m
findRepository5 env = dep env
findController5 :: EnvHKD5 m -> Controller m
findController5 env = dep env

-- Test TheFieldName
type EnvHKD6 :: (Type -> Type) -> Type
data EnvHKD6 m = EnvHKD6
  { loggerx :: (Logger m),
    repositoryx :: (Repository m),
    controllerx :: (Controller m)
  } deriving stock Generic
    
deriving via (TheFieldName "loggerx" (EnvHKD6 m)) instance Has Logger m (EnvHKD6 m)
deriving via (TheFieldName "repositoryx" (EnvHKD6 m)) instance Has Repository m (EnvHKD6 m)
deriving via (TheFieldName "controllerx" (EnvHKD6 m)) instance Has Controller m (EnvHKD6 m)

findLogger6 :: EnvHKD6 m -> Logger m
findLogger6 env = dep env
findRepository6 :: EnvHKD6 m -> Repository m
findRepository6 env = dep env
findController6 :: EnvHKD6 m -> Controller m
findController6 env = dep env


-- Deriving Phased using the barbies library, to check that they are compatible
-- typeclasses

-- newtype B e_ h m = B (e_ h m)

-- instance (FunctorT e_, TraversableT e_, ApplicativeT e_) => Phased (B e_) where
--     traverseH f (B e) = B <$> ttraverse f e
--     liftA2H f (B ax) (B hx) = B $ tmap (\(Pair az hz) -> f az hz) $ tprod ax hx

type EnvHKD7 :: (Type -> Type) -> (Type -> Type) -> Type
data EnvHKD7 h m = EnvHKD7
  { logger :: h (Logger m),
    repository :: h (Repository m),
    controller :: h (Controller m)
  } deriving stock Generic
    deriving anyclass (FunctorT, TraversableT, ApplicativeT)

instance Phased EnvHKD7 where
    traverseH f e = ttraverse f e
    liftA2H f ax hx = tmap (\(Pair az hz) -> f az hz) $ tprod ax hx


-- This is an example of how to combine the "Phased" approach 
-- with DepT.
--
-- Notice that the "allocator" works in IO but the ultimate
-- effect monad is a Writer.
--
-- Also the identity monad is I instead of Identity, 
-- code works with both.
type Allocator = ContT () IO

type Phases = Allocator `Compose` I

allocateMap :: ContT () IO (IORef (Map Int String))
allocateMap = ContT $ bracket (newIORef Map.empty) pure

envHKD :: EnvHKD Phases (DepT (EnvHKD I) (Writer TestTrace))
envHKD =  EnvHKD {
      logger = 
        skipPhase @Allocator $
        pure $ makeFakeLogger
    , repository = 
        allocateMap `bindPhase` \_ -> 
        pure $ makeFakeRepository
    , controller = 
        skipPhase @Allocator $ 
        pure $ makeController
    } 

testEnvHKD :: Assertion
testEnvHKD = do
    runContT (pullPhase @Allocator envHKD) \env -> do
        let r = execWriter $ runDepT (do env <- ask; serve (dep env) 7) env
        assertEqual "" (["I'm going to insert in the db!","I'm going to select an entity","I'm going to write the entity!"],[1,2,3,4]) r

--
-- Phased Approach + DepT, with an InductiveEnv
-- InductiveEnv requires Identity as the final wrapping type, I doesn't work :(

testEnvInductive :: Assertion
testEnvInductive = do
    let envInductive =
              EmptyEnv 
            & AddDep (skipPhase @Allocator $
                      Identity $ makeFakeLogger) 
            & AddDep (allocateMap `bindPhase` \_ -> 
                      Identity $ makeFakeRepository)
            & AddDep (skipPhase @Allocator $ 
                      Identity $ makeController)
    runContT (pullPhase @Allocator envInductive) \env -> do
        let r = execWriter $ runDepT (do env <- ask; serve (dep env) 7) env
        assertEqual "" (["I'm going to insert in the db!","I'm going to select an entity","I'm going to write the entity!"],[1,2,3,4]) r

--
--
--
tests :: TestTree
tests =
  testGroup
    "All"
    [
      testCase "non HKD, pure" $
              assertEqual "" (["I'm going to insert in the db!","I'm going to select an entity","I'm going to write the entity!"],[1,2,3,4]) $
              execWriter $ runDepT (do env <- ask; serve (dep env) 7) env
    , testCase "HKD, phased, uses I, pure" $ testEnvHKD
    , testCase "HKD, InductiveEnv, uses Identity" $ testEnvInductive
    ]

main :: IO ()
main = defaultMain tests
