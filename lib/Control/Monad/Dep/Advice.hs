{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Dep.Advice where

import Control.Monad.Dep

class Advisee ac u c e m r | r -> e m where
  advise ::
    ( forall a . ac a => a -> u ) ->  
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
