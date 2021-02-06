{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Control.Monad.Dep
import Control.Monad.Dep.Has
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Kind
import Data.Coerce
import Data.List (intercalate)
import Rank2 qualified
import Rank2.TH qualified
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (log)

instance DepMarker Logger where
    type PreferredFieldName Logger = "logger" 
    type ExtractedDepType Logger = Logger

newtype Logger d = Logger { runLogger :: String -> d () }

type Env :: (Type -> Type) -> Type
data Env m = Env
  { logger :: Logger m,
    repository :: Int -> m (),
    controller :: Int -> m String
  }

instance Has Logger m (Env m) where

-- mkController :: forall d e m . MonadDep '[Has Logger] d e m => Int -> m String
mkController :: forall d e m . (MonadReader e m, LiftDep d m, Has Logger d e) => Int -> m String
mkController x = do
  e <- ask
  liftD $ runLogger (the @_ @Logger e) "I'm going to insert in the db!" 
  -- liftD $ runLogger (the @_ @Logger e) "I'm going to insert in the db!" 
  return "view"


type EnvIO :: Type
data EnvIO = EnvIO
  { logger :: Logger IO,
    repository :: Int -> IO ()
  }

instance Has Logger IO EnvIO

--
--
tests :: TestTree
tests =
  testGroup
    "All"
    [
    ]

main :: IO ()
main = defaultMain tests
