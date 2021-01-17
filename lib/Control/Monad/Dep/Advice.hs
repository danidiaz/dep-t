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

class Advisee ac a c e m r | r -> e m where
  advise ::
    ( forall argument . ac argument => argument -> a ) ->  
    ( forall x.
      (c (e (DepT e m)) (DepT e m), Monad m) =>
      [a] ->
      DepT e m x ->
      DepT e m x
    ) ->
    r ->
    r

instance (c (e (DepT e m)) (DepT e m), Monad m, m ~ m') => Advisee ac a c e m (DepT e m' x) where
  advise _ f d = f [] d

instance (Advisee ac a c e m r, ac a, Monad m) => Advisee ac a c e m (a -> r) where
  advise argAdaptor f ar =
    let advise' = advise @ac @a @c @e @m @r argAdaptor
     in \a -> advise' (\names d -> f (argAdaptor a : names) d) (ar a)
