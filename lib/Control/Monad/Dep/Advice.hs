{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Dep.Advice where

import Control.Monad.Dep
import Data.Kind

type Advisee ::
  (Type -> Constraint) ->
  Type ->
  (Type -> (Type -> Type) -> Constraint) ->
  ((Type -> Type) -> Type) -> 
  (Type -> Type) ->
  Type ->
  Constraint
class Advisee ac u c e m r | r -> e m where
  advise ::
    (forall a. ac a => a -> u) ->
    ( forall x.
      (c (e (DepT e m)) (DepT e m), Monad m) =>
      [u] ->
      DepT e m x ->
      DepT e m x
    ) ->
    r ->
    r

instance (c (e (DepT e m)) (DepT e m), Monad m) => Advisee ac u c e m (DepT e m x) where
  advise _ f d = f [] d

instance (Advisee ac u c e m r, ac a, Monad m) => Advisee ac u c e m (a -> r) where
  advise argAdaptor f ar =
    let advise' = advise @ac @u @c @e @m @r argAdaptor
     in \a -> advise' (\names d -> f (argAdaptor a : names) d) (ar a)
