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
import Data.SOP
import Data.SOP.NP

type Multicurryable ::
  [Type] ->
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type ->
  Type ->
  Constraint
class Multicurryable as e m r curried | curried -> as e m r where
  type DownToBaseMonad as e m r curried :: Type
  multiuncurry :: curried -> NP I as -> DepT e m r
  multicurry :: (NP I as -> DepT e m r) -> curried
  _runFromEnv :: m (e (DepT e m)) -> (e (DepT e m) -> curried) -> DownToBaseMonad as e m r curried

instance Monad m => Multicurryable '[] e m r (DepT e m r) where
  type DownToBaseMonad '[] e m r (DepT e m r) = m r
  multiuncurry action Nil = action
  multicurry f = f Nil
  _runFromEnv producer extractor = do
    e <- producer
    runDepT (extractor e) e

instance Multicurryable as e m r curried => Multicurryable (a ': as) e m r (a -> curried) where
  type DownToBaseMonad (a ': as) e m r (a -> curried) = a -> DownToBaseMonad as e m r curried
  multiuncurry f (I a :* as) = multiuncurry @as @e @m @r @curried (f a) as
  multicurry f a = multicurry @as @e @m @r @curried (f . (:*) (I a))
  _runFromEnv producer extractor a = _runFromEnv @as @e @m @r @curried producer (\f -> extractor f a)

class Multicurryable as e m r curried => Deceivable as newtyped e m r curried where
  type Deceive as newtyped e m r curried :: Type  
  deceive :: (e (DepT e m) -> newtyped) -> Deceive as newtyped e m r curried -> curried 

instance Monad m => Deceivable '[] newtyped e m r (DepT e m r) where
  type Deceive '[] newtyped e m r (DepT e m r) = ReaderT newtyped m r
  deceive f action = DepT (withReaderT f action)

instance Deceivable as newtyped e m r curried => Deceivable (a ': as) newtyped e m r (a -> curried) where
  type Deceive (a ': as) newtyped e m r (a -> curried) = a -> Deceive as newtyped e m r curried
  deceive f g a = deceive @as @newtyped @e @m @r f (g a)

--
--
--
type HasLogger :: (Type -> Type) -> Type -> Constraint
class HasLogger m em | em -> m where
  logger :: em -> String -> m ()
type HasIntermediate :: (Type -> Type) -> Type -> Constraint
class HasIntermediate m em | em -> m where
  intermediate :: em -> String -> m ()
type Env :: (Type -> Type) -> Type
data Env m = Env
  { _logger1 :: String -> m (),
    _logger2 :: String -> m (),
    _intermediate :: String -> m (),
    _controllerA :: Int -> m (),
    _controllerB :: Int -> m ()
  }
instance HasLogger m (Env m) where
  logger = _logger1
instance HasIntermediate m (Env m) where
  intermediate = _intermediate

newtype Switcheroo m = Switcheroo (Env m) 
    deriving newtype (HasIntermediate m)
instance HasLogger m (Switcheroo m) where
  logger (Switcheroo e) = _logger2 e

envW :: Env (DepT Env (Writer [String]))
envW = Env 
  {
    _logger1 = 
       \_ -> tell ["logger 1"],
    _logger2 = 
       \_ -> tell ["logger 2"],
    _intermediate =
       \_ -> do e <- ask ; liftD $ logger e "foo", 
    _controllerA = 
       \_ -> do e <- ask; liftD $ logger e "foo" ; liftD $ intermediate e "foo",
    _controllerB = 
       deceive Switcheroo $
       \_ -> do e <- ask; liftD $ logger e "foo" ; liftD $ intermediate e "foo"
  }

_controllerA' :: MonadDep '[HasLogger, HasIntermediate] d e m => String -> m () 
_controllerA' = \_ -> do e <- ask; liftD $ logger e "foo" ; liftD $ intermediate e "foo"

_intermediate' :: MonadDep '[HasLogger] d e m => String -> m () 
_intermediate' = \_ -> do e <- ask ; liftD $ logger e "foo" 

--
--
tests :: TestTree
tests =
  testGroup
    "All"
    [
      testCase "undeceived" $
        assertEqual "" ["logger 1", "logger 1"] $
          execWriter $ (do e <- ask; _controllerA e 7) `runDepT` envW,
      testCase "deceived" $
        assertEqual "" ["logger 2", "logger 1"] $
          execWriter $ (do e <- ask; _controllerB e 7) `runDepT` envW
    ]

main :: IO ()
main = defaultMain tests
