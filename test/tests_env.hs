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
{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Control.Monad.Dep
import Control.Monad.Dep.Has
import Control.Monad.Dep.Has.Env
import Control.Monad.Dep.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Functor.Constant
import Data.Functor.Compose
import Data.Coerce
import Data.Kind
import Data.List (intercalate)
import GHC.Generics
import Rank2 qualified
import Rank2.TH qualified
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (log)
import Data.Functor.Identity
import GHC.TypeLits
import Control.Monad.Trans.Cont
import Data.Aeson
import Data.Aeson.Types
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.IORef
import System.IO
import Control.Exception
import Control.Arrow (Kleisli (..))
import Data.Text qualified as Text
import Data.Function ((&))
import Data.Functor ((<&>))

type Logger :: (Type -> Type) -> Type
newtype Logger d = Logger {
    info :: String -> d ()
  }
  deriving stock Generic

data Repository d = Repository
  { findById :: Int -> d (Maybe String)
  , insert :: String -> d Int
  }
  deriving stock Generic

data Controller d = Controller 
  { create :: d Int
  , append :: Int -> String -> d Bool 
  } 
  deriving stock Generic

type MessagePrefix = Text.Text

data LoggerConfiguration = LoggerConfiguration { 
        messagePrefix :: MessagePrefix
    } deriving stock (Show, Generic)
      deriving anyclass FromJSON

makeStdoutLogger :: MonadIO m => MessagePrefix -> env -> Logger m
makeStdoutLogger prefix _ = Logger (\msg -> liftIO (putStrLn (Text.unpack prefix ++ msg)))

allocateMap :: ContT () IO (IORef (Map Int String))
allocateMap = ContT $ bracket (newIORef Map.empty) pure

makeInMemoryRepository 
    :: Has Logger IO env 
    => IORef (Map Int String) 
    -> env 
    -> Repository IO
makeInMemoryRepository ref (asCall -> call) = do
    Repository {
       findById = \key -> do
            call info "I'm going to do a lookup in the map!"
            theMap <- readIORef ref
            pure (Map.lookup key theMap),
       insert = \content -> do 
            call info "I'm going to insert in the map!"
            theMap <- readIORef ref
            let next = Map.size theMap
            writeIORef ref $ Map.insert next content theMap 
            pure next
    }

makeController :: (Has Logger m env, Has Repository m env, Monad m) => env -> Controller m
makeController (asCall -> call) = Controller {
      create = do
          call info "Creating a new empty resource."
          key <- call insert ""
          pure key
    , append = \key extra -> do
          call info "Appending to a resource"
          mresource <- call findById key
          case mresource of
            Nothing -> do
                pure False
            Just resource -> do
                call insert (resource ++ extra) 
                pure True
    }

-- makeController :: MonadDep '[HasLogger, HasRepository] d e m => Int -> m String
-- makeController x = do
--   e <- ask
--   liftD $ logger e "I'm going to insert in the db!"
--   liftD $ repository e x
--   return "view"
-- 

--
-- to test the coercible in the definition of Has
type EnvHKD :: (Type -> Type) -> (Type -> Type) -> Type
data EnvHKD h m = EnvHKD
  { logger :: h (Logger m),
    repository :: h (Repository m),
    controller :: h (Controller m)
  } deriving stock Generic
    deriving anyclass (Phased, DemotableFieldNames, FieldsFindableByType)

deriving via Autowired (EnvHKD Identity m) instance 
    Autowireable (Identity (r_ m)) r_ m (EnvHKD Identity m) => Has r_ m (EnvHKD Identity m)

-- deriving via Autowired (EnvHKD Identity m) instance Has Logger m (EnvHKD Identity m)
-- deriving via Autowired (EnvHKD Identity m) instance Has Repository m (EnvHKD Identity m)
-- deriving via Autowired (EnvHKD Identity m) instance Has Controller m (EnvHKD Identity m)

type Configuration = Kleisli Parser Value 
type Allocation = ContT () IO
type Construction env_ m = ((->) (env_ Identity m)) `Compose` Identity
type Phases env_ m = Configuration `Compose` Allocation `Compose` Construction env_ m

phases :: Configuration (Allocation (Construction env_ m (r_ m))) -> Phases env_ m (r_ m)
phases = coerce

constructor :: forall env_ m r_ . (env_ Identity m -> r_ m) -> Construction env_ m (r_ m)
constructor = coerce

parseConf :: FromJSON a => Configuration a
parseConf = Kleisli parseJSON

env :: EnvHKD (Phases EnvHKD IO) IO
env = EnvHKD {
      logger = 
        phases $ parseConf 
             <&> \conf -> pure @Allocation 
               $ constructor (makeStdoutLogger conf)
    , repository = 
        phases $ pure @Configuration 
               $ allocateMap
             <&> \ref -> constructor (makeInMemoryRepository ref)
    , controller = 
        phases $ pure @Configuration 
               $ pure @Allocation 
               $ constructor makeController
}

--
--
--
tests :: TestTree
tests =
  testGroup
    "All"
    [
        testCase "fieldNames" $
            let fieldNames :: EnvHKD (Compose (Constant String) Identity) IO
                fieldNames = demoteFieldNames
             in
             assertEqual "" "logger repository controller" $
                 let EnvHKD { logger = Compose (Constant loggerField),
                              repository = Compose (Constant repositoryField),  
                              controller = Compose (Constant controllerField)  
                             } = fieldNames
                  in intercalate " " [loggerField, repositoryField, controllerField]
    ]

main :: IO ()
main = defaultMain tests
