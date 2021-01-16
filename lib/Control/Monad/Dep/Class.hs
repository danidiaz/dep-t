{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Dep.Class where

import Control.Monad.Reader
import Data.Kind

-- Code taken and modified from these SO answers from Li-yao Xia and dfeuer:
-- https://stackoverflow.com/a/61648924/1364288

class (MonadReader e m, m ~ TheMonad r) => Call e m r where
  type TheMonad r :: Type -> Type
  call :: (e -> r) -> r

type Call' :: Type -> (Type -> Type) -> Type -> Bool -> Constraint
class (MonadReader e m, m ~ TheMonad' r b) => Call' e m r b where
  type TheMonad' r b :: Type -> Type
  call' :: (e -> r) -> r

type IsFunction :: Type -> Bool
type family IsFunction t where
    IsFunction ((->) _ _) = True
    -- put a type error here, for non-type constructors which won't match at all
    IsFunction _ = False

-- instance Call' e m ( r => Call e m r where
--     call' :: (e -> r) -> r


-- class (MonadReader e m, e ~ TheEnv r, m ~ TheMonad r) => Call e m e' r | r -> e' where
--   type TheMonad r :: Type -> Type
--   call :: (e -> r) -> r

instance Call e m r => Call e m (a -> r) where
  type TheMonad (a -> r) = TheMonad r
  call f x = call (\env -> f env x)

-- instance (e ~ e', m ~ m', MonadReader e' m') => Call e m (m' x) where
--   type TheMonad (m' x) = m'
--   call f = ask >>= f
