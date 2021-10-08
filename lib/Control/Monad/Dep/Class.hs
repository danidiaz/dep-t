{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module provides definitions that let us translate record-of-functions code of the form:
--
-- >>> :{ 
--  type HasLogger :: (Type -> Type) -> Type -> Constraint
--  class HasLogger d e | e -> d where
--    logger :: e -> String -> d ()
--  type HasRepository :: (Type -> Type) -> Type -> Constraint
--  class HasRepository d e | e -> d where
--    repository :: e -> Int -> d ()
--  mkControllerIO :: (HasLogger IO e, HasRepository IO e) => Int -> ReaderT e IO String
--  mkControllerIO x = do
--    e <- ask
--    liftIO $ logger e "I'm going to insert in the db!"
--    liftIO $ repository e x
--    return "view"
-- :}
--
-- into the more polymorphic form:
--
-- >>> :{
--  mkController :: MonadDep [HasLogger, HasRepository] d e m => Int -> m String
--  mkController x = do
--    e <- ask
--    liftD $ logger e "I'm going to insert in the db!"
--    liftD $ repository e x
--    return "view"
-- :}
--
-- which can also be given the equivalent signature:
--
-- >>> :{
--    mkController' :: (HasLogger d e, HasRepository d e, LiftDep d m, MonadReader e m) => Int -> m String
--    mkController' = mkController
-- :}   
--
-- The new code can be used as a drop-in replacement of the old one: 
--
-- >>> :{ 
--    mkControllerIO' :: (HasLogger IO e, HasRepository IO e) => Int -> ReaderT e IO String
--    mkControllerIO' = mkController'
-- :}
--
-- Notice that in the new code effects taken from the environment record are
-- lifted using 'liftD' instead of 'lift' or 'liftIO'. 
--
module Control.Monad.Dep.Class
  ( -- * Reader-like monads carrying dependencies in their environment
    MonadDep,

    -- * Lifting effects from dependencies
    LiftDep (..),
    -- * Helpers
    useEnv
  )
where

import Control.Monad.Reader
import Data.Kind

-- $setup
--
-- >>> :set -XTypeApplications
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XImportQualifiedPost
-- >>> :set -XTemplateHaskell
-- >>> :set -XStandaloneKindSignatures
-- >>> :set -XNamedFieldPuns
-- >>> :set -XFunctionalDependencies
-- >>> :set -XFlexibleContexts
-- >>> :set -XDataKinds
-- >>> import Control.Monad.Reader
-- >>> import Control.Monad.Dep
-- >>> import Rank2 qualified
-- >>> import Rank2.TH qualified
--


-- | Auxiliary typeclass for monads that can be lifted to other monads.
--
-- Its intended use is lifting monadic actions found in some reader-like
-- environment.
--
-- __Note:__ The @RIO@ monad from <http://hackage.haskell.org/package/rio rio> could be
-- given a @LiftDept IO RIO@ instance; it's not done here to avoid increasing
-- the dependency footprint.
type LiftDep :: (Type -> Type) -> (Type -> Type) -> Constraint
class Monad d => LiftDep d m where
  liftD :: d x -> m x

-- | The simplest case: we can d-lift the base monad sitting just below a 'ReaderT'.
--
-- (Perhaps this could be extended to any monad transformer, but let's leave it simple for now.)
instance Monad m => LiftDep m (ReaderT e m) where
  liftD = lift

-- Trying to write a general "lift from anywhere in the stack" like
-- instance (Monad m, LiftDep below m, MonadTrans t) => LiftDep below (t m)
-- caused OverlappedInstances problems with DepT.
-- Lot let's not try to be *excessively* general.

-- -- | Any monad can be \"lifted\" to itself.
-- Better limit this to DepT
-- instance LiftDep m m where
--   liftD = id

-- | 'MonadDep' is not its own typeclass, but 'MonadReader' plus some supplementary constraints. 
--
-- A @MonadDep dependencies d e m@ is a @MonadReader e m@ where the environment
-- @e@ provides some @dependencies@ with effects in the monad @d@, additionally requiring that the 
-- effects can be lifted back to the monad @m@ by using 'liftD'.
--
-- The @dependencies@ are specified as a type-level list of two-parameter
-- @HasX@ typeclasses. Those typeclasses should expect the effect monad
-- @d@ as its first parameter.
--
-- Writing code polymorphic over 'MonadDep' lets us execute it in both 'ReaderT' and 'Control.Monad.Dep.DepT' contexts.
type MonadDep ::
  [(Type -> Type) -> Type -> Constraint] ->
  (Type -> Type) ->
  Type ->
  (Type -> Type) ->
  Constraint
type family MonadDep dependencies d e m where
  MonadDep '[] d e m = (LiftDep d m, MonadReader e m)
  MonadDep (dependency ': dependencies) d e m = (dependency d e, MonadDep dependencies d e m)

-- | Avoids repeated calls to 'liftD' when all the effects in a function come from the environment.
useEnv :: forall d e m r. (LiftDep d m, MonadReader e m) => (e -> d r) -> m r
useEnv f = do
  e <- ask
  liftD (f e)

