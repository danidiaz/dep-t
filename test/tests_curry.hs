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

tests :: TestTree
tests =
  testGroup
    "All"
    [
    ]

main :: IO ()
main = defaultMain tests
