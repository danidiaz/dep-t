{-# language MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}
module Control.Monad.Dep.Class where

import Control.Monad.Reader

-- Code taken and modified from these SO answers from Li-yao Xia and dfeuer:
-- https://stackoverflow.com/a/61648924/1364288

class e ~ TheEnv r => Call e r where
  type TheEnv r
  call :: (e -> r) -> r

instance Call e r => Call e (a -> r) where
  type TheEnv (a -> r) = TheEnv r
  call f x = call (\env -> f env x)

instance (Monad m, e ~ e') => Call e (ReaderT e' m r) where
  type TheEnv (ReaderT e' m r) = e'
  call f = ask >>= f
