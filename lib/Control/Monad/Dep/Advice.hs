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

--
--
--
type Advisee ::
  (Type -> (Type -> Type) -> Constraint) ->
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type ->
  Constraint
class Advisee c e m r | r -> e m where
  advise ::
    ( forall x.
      (c (e (DepT e m)) (DepT e m), Monad m) =>
      DepT e m x ->
      DepT e m x
    ) ->
    r ->
    r

instance (c (e (DepT e m)) (DepT e m), Monad m) => Advisee c e m (DepT e m x) where
  advise advice d = advice d

instance (Advisee c e m r) => Advisee c e m (a -> r) where
  advise advice f a = advise @c @e @m @r advice (f a)

--
--
--
type ArgAwareAdvisee ::
  (Type -> Constraint) ->
  Type ->
  (Type -> (Type -> Type) -> Constraint) ->
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type ->
  Constraint
class ArgAwareAdvisee ac u c e m r | r -> e m where
  adviseWithArgs ::
    ( forall a.
      ac a =>
      a ->
      u
    ) ->
    ( forall x.
      (c (e (DepT e m)) (DepT e m), Monad m) =>
      [u] ->
      DepT e m x ->
      DepT e m x
    ) ->
    r ->
    r

instance (c (e (DepT e m)) (DepT e m), Monad m) => ArgAwareAdvisee ac u c e m (DepT e m x) where
  adviseWithArgs _ advice d = advice [] d

instance (ArgAwareAdvisee ac u c e m r, ac a) => ArgAwareAdvisee ac u c e m (a -> r) where
  adviseWithArgs argAdaptor advice ar =
    let adviseWithArgs' = adviseWithArgs @ac @u @c @e @m @r argAdaptor
     in \a -> adviseWithArgs' (\args d -> advice (argAdaptor a : args) d) (ar a)

