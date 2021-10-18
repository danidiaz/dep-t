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
import Control.Monad.Dep.Env
import Control.Monad.Dep.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Functor.Constant
import Data.Functor.Compose
import Data.Coerce
import Data.Kind
import Data.List (intercalate)
import GHC.Generics (Generic)
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
import Data.ByteString.Lazy qualified as Bytes
import Data.Function ((&))
import Data.Functor ((<&>), ($>))
import Data.String

type Logger :: (Type -> Type) -> Type
newtype Logger d = Logger {
    info :: String -> d ()
  }
  deriving stock Generic

data Repository d = Repository
  { findById :: Int -> d (Maybe String)
  , putById :: Int -> String -> d ()
  , insert :: String -> d Int
  }
  deriving stock Generic

data Controller d = Controller 
  { create :: d Int
  , append :: Int -> String -> d Bool 
  , inspect :: Int -> d (Maybe String)
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
            pure (Map.lookup key theMap)
       , putById = \key content -> do
            theMap <- readIORef ref
            writeIORef ref $ Map.insert key content theMap 
       , insert = \content -> do 
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
                call putById key (resource ++ extra) 
                pure True
    , inspect = \key -> do
          call findById key 
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

deriving via Autowired (EnvHKD Identity m) instance Autowireable r_ m (EnvHKD Identity m) => Has r_ m (EnvHKD Identity m)

-- deriving via Autowired (EnvHKD Identity m) instance Has Logger m (EnvHKD Identity m)
-- deriving via Autowired (EnvHKD Identity m) instance Has Repository m (EnvHKD Identity m)
-- deriving via Autowired (EnvHKD Identity m) instance Has Controller m (EnvHKD Identity m)

type Configurator = Kleisli Parser Value 
type Allocator = ContT () IO
type Constructor env_ m = ((->) (env_ Identity m)) `Compose` Identity
type Phases env_ m = Configurator `Compose` Allocator `Compose` Constructor env_ m

phases :: Configurator (Allocator (Constructor env_ m (r_ m))) -> Phases env_ m (r_ m)
phases = coerce

constructor :: forall env_ m r_ . (env_ Identity m -> r_ m) -> Constructor env_ m (r_ m)
constructor = coerce

parseConf :: FromJSON a => Configurator a
parseConf = Kleisli parseJSON

env :: EnvHKD (Phases EnvHKD IO) IO
env = EnvHKD {
      logger = 
        Compose $ parseConf <&> \(LoggerConfiguration {messagePrefix}) -> 
        Compose $ pure @Allocator () <&> \_ ->
        constructor (makeStdoutLogger messagePrefix)
--        phases $ parseConf <&> \(LoggerConfiguration {messagePrefix}) 
--              -> pure @Allocator 
--               $ constructor (makeStdoutLogger messagePrefix)
    , repository = 
        phases $ pure @Configurator () <&> \_ ->
                 allocateMap <&> \ref ->
                 constructor (makeInMemoryRepository ref)
    , controller = 
        phases $ pure @Configurator 
               $ pure @Allocator 
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
            let fieldNames :: EnvHKD (Constant String) IO
                fieldNames = demoteFieldNames
             in
             assertEqual "" "logger repository controller" $
                 let EnvHKD { logger = Constant loggerField
                            , repository = Constant repositoryField  
                            , controller = Constant controllerField  
                            } = fieldNames
                  in intercalate " " [loggerField, repositoryField, controllerField]
        , testCase "environmentConstruction" $ do
            let parseResult = eitherDecode' (fromString "{ \"logger\" : { \"messagePrefix\" : \"[foo]\" }, \"repository\" : null, \"controller\" : null }")
            print parseResult 
            let Right value = parseResult 
                Kleisli parser = 
                      pullPhase  
                    $ mapPhaseWithFieldNames 
                        (\fieldName (Kleisli f) -> Kleisli \o -> explicitParseField f o (fromString fieldName)) 
                    $ env
                parseResult' = parseEither (withObject "configuration" parser) value 
            print $ case parseResult' of
                Left x -> x
                Right _ -> ""
            let Right allocators = parseResult'
            runContT (pullPhase allocators) \constructors -> do
                let (asCall -> call) = fixEnv constructors
                resourceId <- call create
                call append resourceId "foo"
                call append resourceId "bar"
                x <- call inspect resourceId
                print x
                Just result <- call inspect resourceId
                assertEqual "" "foobar" $ result
    ]

main :: IO ()
main = defaultMain tests
