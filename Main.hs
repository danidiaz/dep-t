{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Monad.Dep
import Control.Monad.Reader
import Data.Kind
import Rank2 qualified
import Rank2.TH qualified

-- The environment doesn't know about any concrete monad
type Env :: (Type -> Type) -> Type
data Env m = Env
  { logger :: String -> m (),
    logic :: Int -> m Int
  }

$(Rank2.TH.deriveFunctor ''Env)

env :: Env (DepT Env IO)
env =
  Env
    { logger = \msg -> liftIO (putStrLn msg),
      logic = \x -> return $ x*x
    }

env' :: Env (DepT Env IO)
env' = zoomEnv (Rank2.<$>) id env

main :: IO ()
main = do
  r <- runDepT (logic env' 7) env'
  print r
