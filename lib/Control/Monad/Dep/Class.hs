{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Dep.Class where

import Control.Monad.Reader
import Data.Kind

-- Code taken and modified from these SO answers from Li-yao Xia and dfeuer:
-- https://stackoverflow.com/a/61648924/1364288

class (MonadReader e m, m ~ TheMonad r) => Call e m r where
  type TheMonad r :: Type -> Type
  call :: (e -> r) -> r

-- class (MonadReader e m, e ~ TheEnv r, m ~ TheMonad r) => Call e m e' r | r -> e' where
--   type TheMonad r :: Type -> Type
--   call :: (e -> r) -> r

instance Call e m r => Call e m (a -> r) where
  type TheMonad (a -> r) = TheMonad r
  call f x = call (\env -> f env x)

instance (e ~ e', m ~ m', MonadReader e' m') => Call e m (m' x) where
  type TheMonad (m' x) = m'
  call f = ask >>= f
