{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}

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

-- Has-style typeclasses can be provided to avoid depending on concrete
-- environments.
-- Note that the environment determines the monad.
type HasLogger :: Type -> (Type -> Type) -> Constraint
class HasLogger r m | r -> m where
  getLogger :: r -> String -> m ()

-- If our environment is parmeterized by the monad m, then logging is done in
-- m.
instance HasLogger (Env m) m where
  getLogger = logger

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

-- We select "logic" as the entrypoint and run it.
result :: IO Int
result = runDepT (logic env 7) env

-- An attempt with ReaderT which doesn't work
-- env' =
--   Env
--     { logger = __logic logger
--     }
-- 
-- result' :: IO Int
-- result' = runReaderT (logic env' 7) env'


-- The environment doesn't know about any concrete monad
type BiggerEnv :: (Type -> Type) -> Type
data BiggerEnv m = BiggerEnv
  { inner :: Env m,
    extra :: Int -> m Int
  }

$(Rank2.TH.deriveFunctor ''BiggerEnv)

biggerEnv :: BiggerEnv (DepT BiggerEnv IO)
biggerEnv = BiggerEnv 
    {
        -- inner = (Rank2.<$>) (withDepT (Rank2.<$>) inner) env,
        inner = zoomEnv (Rank2.<$>) inner env,
        extra = pure 
    }

main :: IO ()
main = do
    r <- runDepT ((logic . inner $ biggerEnv) 7) biggerEnv
    print r

