{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Main (main) where

import Control.Monad.Dep
import Control.Monad.Reader
import Data.Kind

-- The environment doesn't know about any concrete monad
type Env :: (Type -> Type) -> Type
data Env m = Env
  { logger :: String -> m (),
    logic :: Int -> m Int
  }

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
-- This one because it only needs MonadIO.
_logger :: MonadIO m => String -> m ()
_logger msg = liftIO (putStrLn msg)

-- This one because of the axiliary HasLogger class.
_logic :: (MonadReader e m, HasLogger e m) => Int -> m Int
_logic x = do
  logger <- reader getLogger
  logger "I'm going to multiply a number by itself!"
  return $ x * x

--
-- This is the first time DepT is used in this module
env :: Env (DepT Env IO)
env =
  Env
    { logger = _logger,
      logic = _logic
    }

-- We select "logic" as the entrypoint and run it.
result :: IO Int
result = runDepT (logic env 7) env

main :: IO ()
main = putStrLn "Test suite not yet implemented."
