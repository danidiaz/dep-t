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

--
-- to test the coercible in the definition of Has
type EnvHKD :: (Type -> Type) -> (Type -> Type) -> Type
data EnvHKD h m = EnvHKD
  { logger :: h (Logger m),
    repository :: h (Repository m),
    controller :: h (Controller m)
  } deriving Generic

deriving anyclass instance Phased EnvHKD
deriving anyclass instance DemotableFieldNames EnvHKD

deriving anyclass instance Has Logger m (EnvHKD Identity m)
deriving anyclass instance Has Repository m (EnvHKD Identity m)
deriving anyclass instance Has Controller m (EnvHKD Identity m)

fieldNames :: EnvHKD (Compose (Constant String) Identity) IO
fieldNames = demoteFieldNames


--
--
--
tests :: TestTree
tests =
  testGroup
    "All"
    [
        testCase "fieldNames" $
        assertEqual "" "logger repository controller" $
            let EnvHKD { logger = Compose (Constant loggerField),
                         repository = Compose (Constant repositoryField),  
                         controller = Compose (Constant controllerField)  
                        } = fieldNames
             in intercalate " " [loggerField, repositoryField, controllerField]
    ]

main :: IO ()
main = defaultMain tests
