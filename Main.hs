{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
-- {-# LANGUAGE RankNTypes #-}

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

-- These two functions don't know the concrete envionment record.
--
-- This one because it only needs MonadIO.
_logger :: MonadIO m => String -> m ()
_logger msg = liftIO (putStrLn msg)

-- This one because it receives a getter for the logger
-- A HasX-style typeclass would have been an alternative. 
_logic :: MonadReader e m => (e -> String -> m ()) -> Int -> m Int
_logic getLogger x = do
  logger <- reader getLogger
  logger "I'm going to multiply a number by itself!"
  return $ x * x

-- This is the first time DepT is used in this module.
-- Note that it is only here where we settle for IO.
env :: Env (DepT Env IO)
env =
  Env
    { logger = _logger,
      logic = _logic logger
    }

-- The environment doesn't know about any concrete monad
type BiggerEnv :: (Type -> Type) -> Type
data BiggerEnv m = BiggerEnv
  { inner :: Env m,
    extra :: Int -> m Int
  }

biggerEnv :: BiggerEnv (DepT BiggerEnv IO)
biggerEnv = BiggerEnv 
    {
        inner = (zoomEnv (Rank2.<$>) inner env),
        extra = pure 
    }

main :: IO ()
main = do
    r <- runDepT ((logic . inner $ biggerEnv) 7) biggerEnv
    print r

