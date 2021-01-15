{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.Dep
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Kind
import Rank2 qualified
import Rank2.TH qualified
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (log)

-- Some helper typeclasses.
--
-- Has-style typeclasses can be provided to avoid depending on concrete
-- environments.
-- Note that the environment determines the monad.
type HasLogger :: Type -> (Type -> Type) -> Constraint
class HasLogger r m | r -> m where
  logger :: r -> String -> m ()

type HasRepository :: Type -> (Type -> Type) -> Constraint
class HasRepository r m | r -> m where
  repository :: r -> Int -> m ()

-- Some possible implementations.
--
-- An implementation of the controller, done programming against interfaces
-- (well, against typeclasses).
-- Polymorphic on the monad.
mkController :: (MonadReader e m, HasLogger e m, HasRepository e m) => Int -> m Int
mkController x = do
  doLog <- asks logger
  doLog "I'm going to insert in the db!"
  insert <- asks repository
  insert x
  return $ x * x

-- A "real" implementation that interacts with the external world.
mkStdoutLogger :: MonadIO m => String -> m ()
mkStdoutLogger msg = liftIO (putStrLn msg)

-- The traces we accumulate from the fakes during tests
type TestTrace = ([String], [Int])

-- A "fake". A pure implementation for tests.
mkFakeLogger :: MonadWriter TestTrace m => String -> m ()
mkFakeLogger msg = tell ([msg], [])

-- Ditto.
mkFakeRepository :: (MonadReader e m, HasLogger e m, MonadWriter TestTrace m) => Int -> m ()
mkFakeRepository entity = do
  doLog <- asks logger
  doLog "I'm going to write the entity!"
  tell ([], [entity])

-- Here we define some environments, which are basically records-of-functions
-- parameterized by an effect monad.
--
type Env :: (Type -> Type) -> Type
data Env m = Env
  { _logger :: String -> m (),
    _repository :: Int -> m (),
    _controller :: Int -> m Int
  }

$(Rank2.TH.deriveFunctor ''Env)

-- If our environment is parmeterized by the monad m, then logging is done in
-- m.
instance HasLogger (Env m) m where
  logger = _logger

instance HasRepository (Env m) m where
  repository = _repository

-- This bigger environment is for demonstrating how to "nest" environments.
type BiggerEnv :: (Type -> Type) -> Type
data BiggerEnv m = BiggerEnv
  { _inner :: Env m,
    _extra :: Int -> m Int
  }

$(Rank2.TH.deriveFunctor ''BiggerEnv)

--
--
-- Creating environment values and commiting to a concrete monad.
--
-- This is the first time DepT is used in this module.
-- Note that it is only here where we settle for a concrete monad.
env :: Env (DepT Env (Writer TestTrace))
env =
  let _logger = mkFakeLogger
      _controller = mkController
      _repository = mkFakeRepository
   in Env {_logger, _controller, _repository}

biggerEnv :: BiggerEnv (DepT BiggerEnv (Writer TestTrace))
biggerEnv =
  let -- We embed the small environment into the bigger one using "zoomEnv"
      -- and the rank-2 fmap that allows us to change the monad which
      -- parameterized the environment.
      --
      -- _inner' = (Rank2.<$>) (withDepT (Rank2.<$>) inner) env,
      _inner' = zoomEnv (Rank2.<$>) _inner env
      _extra = pure
   in BiggerEnv {_inner = _inner', _extra}

expected :: TestTrace
expected = (["I'm going to insert in the db!", "I'm going to write the entity!"], [7])

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "hopeThisWorks" $
        assertEqual "" expected $
          execWriter $ runDepT ((_controller . _inner $ biggerEnv) 7) biggerEnv
    ]

main :: IO ()
main = defaultMain tests
