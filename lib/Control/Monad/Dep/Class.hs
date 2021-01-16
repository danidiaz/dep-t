{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Dep.Class (MonadDep(..)) where

import Control.Monad.Reader
import Data.Kind

-- Code taken and modified from these SO answers from Li-yao Xia and dfeuer:
-- https://stackoverflow.com/a/61648924/1364288

class MonadReader e m => MonadDep function e m where
    call :: (e -> function) -> function

-- As a warm-up, MonadDep instance for ReaderT.
instance (Monad m, e ~ e', m ~ m') => MonadDep (ReaderT e' m' x) e (ReaderT e m) where
  call f = ask >>= f

instance (MonadDep rest e (ReaderT e m)) => MonadDep (a -> rest) e (ReaderT e m) where
  call f x = call @rest @e @(ReaderT e m) (\z -> f z x)

-- class (MonadReader e m, m ~ TheMonad r) => Call e m r where
--   type TheMonad r :: Type -> Type
--   call :: (e -> r) -> r
-- 
-- type Call' :: Type -> (Type -> Type) -> Type -> Bool -> Constraint
-- class (MonadReader e m, m ~ TheMonad' r b) => Call' e m r b where
--   type TheMonad' r b :: Type -> Type
--   call' :: (e -> r) -> r
-- 
-- type IsFunction :: Type -> Bool
-- type family IsFunction t where
--     IsFunction ((->) _ _) = True
--     -- put a type error here, for non-type constructors which won't match at all
--     IsFunction _ = False
-- 
-- instance (MonadReader e m, Call' e m r (IsFunction r)) => Call e m r where
--     type TheMonad r = TheMonad' r (IsFunction r)
--     call = call' @e @m @r @(IsFunction r)
-- 
-- instance Call e m r => Call' e m (a -> r) True where
--   type TheMonad' (a -> r) True = TheMonad r
--   call' f x = call (\env -> f env x)
-- 
-- instance (e ~ e', m ~ m', MonadReader e' m') => Call' e m (m' x) False where
--   type TheMonad' (m' x) False = m'
--   call' f = ask >>= f
-- 
