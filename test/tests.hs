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
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Control.Monad.Dep
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

-- Some helper typeclasses.
--
-- Has-style typeclasses can be provided to avoid depending on concrete
-- environments.
-- Note that the environment determines the monad.
type HasLogger :: (Type -> Type) -> Type -> Constraint
class HasLogger d e | e -> d where
  logger :: e -> String -> d ()

-- Possible convenience function to avoid having to use ask before logging
-- Worth the extra boilerplate, or not?
--logger' :: (MonadReader e m, LiftDep d m, HasLogger e d) => String -> m ()
loggerD :: MonadDep '[HasLogger] d e m => String -> m ()
loggerD msg = asks logger >>= \f -> liftD $ f msg

type HasRepository :: (Type -> Type) -> Type -> Constraint
class HasRepository d e | e -> d where
  repository :: e -> Int -> d ()

-- Some possible implementations.
--
-- An implementation of the controller, done programming against interfaces
-- (well, against typeclasses).
-- Polymorphic on the monad.
--mkController :: (MonadReader e m, LiftDep d m, HasLogger e d, HasRepository e d) => Int -> m String
mkController :: MonadDep '[HasLogger, HasRepository] d e m => Int -> m String
mkController x = do
  e <- ask
  liftD $ logger e "I'm going to insert in the db!"
  liftD $ repository e x
  return "view"

usesLoggerD :: MonadDep '[HasLogger, HasRepository] d e m => Int -> m String
usesLoggerD i = do
  loggerD "I'm calling the logger!"
  return "foo"

-- A "real" logger implementation that interacts with the external world.
mkStdoutLogger :: MonadIO m => String -> m ()
mkStdoutLogger msg = liftIO (putStrLn msg)

-- A "real" repository implementation
--mkStdoutRepository :: (MonadReader e m, LiftDep d m, HasLogger e d, MonadIO m) => Int -> m ()
mkStdoutRepository :: (MonadDep '[HasLogger] d e m, MonadIO m) => Int -> m ()
mkStdoutRepository entity = do
  e <- ask
  liftD $ logger e "I'm going to write the entity!"
  liftIO $ print entity

-- The traces we accumulate from the fakes during tests
type TestTrace = ([String], [Int])

-- A "fake". A pure implementation for tests.
mkFakeLogger :: MonadWriter TestTrace m => String -> m ()
mkFakeLogger msg = tell ([msg], [])

-- Ditto.
--mkFakeRepository :: (MonadReader e m, LiftDep d m, HasLogger e d, MonadWriter TestTrace m) => Int -> m ()
mkFakeRepository :: (MonadDep '[HasLogger] d e m, MonadWriter TestTrace m) => Int -> m ()
mkFakeRepository entity = do
  e <- ask
  liftD $ logger e "I'm going to write the entity!"
  tell ([], [entity])

--
--
-- Here we define a monomorphic environment working on IO
type EnvIO :: Type
data EnvIO = EnvIO
  { _loggerIO :: String -> IO (),
    _repositoryIO :: Int -> IO ()
  }

envIO' :: EnvIO
envIO' =
  let _loggerIO = mkStdoutLogger
      _repositoryIO i = print "this is the repo"
   in EnvIO {_loggerIO, _repositoryIO}

instance HasLogger IO EnvIO where
  logger = _loggerIO

instance HasRepository IO EnvIO where
  repository = _repositoryIO

-- mkController works both with DepT and with ReaderT + monomorphic environment
runningTheControllerInReaderT :: IO String
runningTheControllerInReaderT = mkController 5 `runReaderT` envIO'

mkControllerIO' :: (HasLogger e IO, HasRepository e IO) => Int -> ReaderT e IO String
mkControllerIO' = mkController

-- In the monomorphic environment, the controller function lives "separate",
-- having access to the logger and the repository through the ReaderT
-- environment.
--
-- The question is: the repository function *also* needs to know about the
-- logger!  Shouldn't it be aware of the ReaderT environment as well? Why
-- privilege the controller function in such a manner?
--
-- In a sufficiently complex app, the diverse functions will form a DAG of
-- dependencies between each other. So it would be nice if the functions were
-- treated uniformly, all having access to (views of) the environment record.
--
-- no need for this now that we have MonadDep... just use the conventional mkController
mkControllerIO :: (HasLogger IO e, HasRepository IO e) => Int -> ReaderT e IO String
mkControllerIO x = do
  e <- ask
  liftIO $ logger e "I'm going to insert in the db!"
  liftIO $ repository e x
  return "view"

--
--
-- Here we define some polymorphic environments, which are basically
-- records-of-functions parameterized by an effect monad.
type Env :: (Type -> Type) -> Type
data Env m = Env
  { _logger :: String -> m (),
    _repository :: Int -> m (),
    _controller :: Int -> m String
  }

$(Rank2.TH.deriveFunctor ''Env)

-- If our environment is parmeterized by the monad m, then logging is done in
-- m.
instance HasLogger m (Env m) where
  logger = _logger

instance HasRepository m (Env m) where
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
-- Note that it is only here where we settle for a concrete monad for the
-- polymorphic environments.
env :: Env (DepT Env (Writer TestTrace))
env =
  let _logger = mkFakeLogger
      _repository = mkFakeRepository
      _controller = mkController
   in Env {_logger, _repository, _controller}

-- An IO variant
envIO :: Env (DepT Env IO)
envIO =
  let _logger = mkStdoutLogger
      _repository = mkStdoutRepository
      _controller = mkController
   in Env {_logger, _repository, _controller}

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

biggerEnvIO :: BiggerEnv (DepT BiggerEnv IO)
biggerEnvIO =
  let _inner' = zoomEnv (Rank2.<$>) _inner envIO
      _extra = pure
   in BiggerEnv {_inner = _inner', _extra}

expected :: TestTrace
expected = (["I'm going to insert in the db!", "I'm going to write the entity!"], [7])

--
--
-- Experiment about adding instrumetation
class Instrumentable e m r | r -> e m where
  instrument ::
    ( forall x.
      HasLogger (DepT e m) (e (DepT e m)) =>
      [String] ->
      DepT e m x ->
      DepT e m x
    ) ->
    r ->
    r

instance HasLogger (DepT e m) (e (DepT e m)) => Instrumentable e m (DepT e m x) where
  instrument f d = f [] d

instance (Instrumentable e m r, Show a) => Instrumentable e m (a -> r) where
  instrument f ar =
    let instrument' = instrument @e @m @r
     in \a -> instrument' (\names d -> f (show a : names) d) (ar a)

instrumentedEnv :: Env (DepT Env (Writer TestTrace))
instrumentedEnv =
  let loggingAdvice args action = do
        e <- ask
        logger e $ "advice before: " ++ intercalate "," args
        r <- action
        logger e $ "advice after"
        pure r
   in env {_controller = instrument loggingAdvice (_controller env)}

expectedInstrumented :: TestTrace
expectedInstrumented = (["advice before: 7", "I'm going to insert in the db!", "I'm going to write the entity!", "advice after"], [7])


--
--
--
boringAction :: DepT NilEnv IO ()
boringAction = lift (putStrLn "")
--
--
--

--
--
-- checking that coerce works on environments...
newtype NilEnv' m = NilEnv' (NilEnv m)

toBeCoerced :: DepT NilEnv IO ()
toBeCoerced = pure ()

toBeCoerced' :: DepT NilEnv' IO ()
toBeCoerced' = coerce toBeCoerced

toBeCoerced'' :: DepT NilEnv IO ()
toBeCoerced'' = coerce toBeCoerced'
--
--
--


tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "hopeThisWorks" $
        assertEqual "" expected $
          execWriter $ runDepT (do e <- ask; (_controller . _inner) e 7) biggerEnv,
      testCase "hopeAOPWorks" $
        assertEqual "" expectedInstrumented $
          execWriter $ runDepT (do e <- ask; _controller e 7) instrumentedEnv,
      testCase "hopeLoggerDWorks" $
        assertEqual "" (["foo"],[]) $
          execWriter $ loggerD "foo" `runDepT` env
    ]

main :: IO ()
main = defaultMain tests
