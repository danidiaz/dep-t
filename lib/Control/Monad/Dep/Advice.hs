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
module Control.Monad.Dep.Advice where

import Control.Monad.Dep

class Instrumentable ac at c e m r | r -> e m where
  instrument ::
    ( forall arg . ac arg => arg -> at ) ->  
    ( forall x.
      c (e (DepT e m)) (DepT e m) =>
      [at] ->
      DepT e m x ->
      DepT e m x
    ) ->
    r ->
    r

instance c (e (DepT e m)) (DepT e m) => Instrumentable ac at c e m (DepT e m x) where
  instrument _ f d = f [] d

instance (Instrumentable ac at c e m r, ac a) => Instrumentable ac at c e m (a -> r) where
  instrument argAdaptor f ar =
    let instrument' = instrument @ac @at @c @e @m @r argAdaptor
     in \a -> instrument' (\names d -> f (argAdaptor a : names) d) (ar a)
