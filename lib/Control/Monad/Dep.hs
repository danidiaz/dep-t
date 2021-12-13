{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

-- |
--    This module provides 'DepT', a monad transformer similar to 'ReaderT'.
--
--    The difference is that the environment of 'DepT' must be parameterized by
--    @DepT@'s own monad stack.
--
--    There's a function 'withDepT' which is analogous to 'withReaderT'.
--    There's no analogue of 'mapReaderT' however. 
module Control.Monad.Dep
  ( 
    -- * The DepT transformer
    DepT (DepT),
    runDepT,
    toReaderT,
    withDepT,
    zoomEnv,
    -- * The simplest environment
    NilEnv(NilEnv),
    -- * The next simplest environment
    -- $constant
    Constant(..),
    -- * Re-exports
    module Control.Monad.Trans,
    module Control.Monad.Dep.Class,
    module Control.Monad.Reader.Class
  )
where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Identity
import Control.Monad.Writer.Class
import Control.Monad.Zip
import Control.Monad.Dep.Class
import Data.Kind (Type)
import Data.Coerce
import Data.Functor.Constant

-- $setup
--
-- >>> :set -XTypeApplications
-- >>> :set -XImportQualifiedPost
-- >>> :set -XTemplateHaskell
-- >>> :set -XStandaloneKindSignatures
-- >>> :set -XNamedFieldPuns
-- >>> import Control.Monad.Dep
-- >>> import Rank2 qualified
-- >>> import Rank2.TH qualified
--


-- |
--    A monad transformer which adds a read-only environment to the given monad.
--    The environment type must be parameterized with the transformer's stack.
--
--    The 'return' function ignores the environment, while @>>=@ passes the
--    inherited environment to both subcomputations.
type DepT ::
  ((Type -> Type) -> Type) ->
  (Type -> Type) ->
  Type ->
  Type
newtype DepT (e_ :: (Type -> Type) -> Type) (m :: Type -> Type) (r :: Type) = DepT {toReaderT :: ReaderT (e_ (DepT e_ m)) m r}
  deriving
    ( Functor,
      Applicative,
      Alternative,
      Monad,
      MonadFix,
      MonadFail,
      MonadZip,
      MonadPlus,
      MonadCont,
      MonadIO,
      MonadUnliftIO,
      MonadReader (e_ (DepT e_ m))
    )

instance MonadTrans (DepT e_) where
  lift = DepT . lift

deriving instance MonadState s m => MonadState s (DepT e_ m)

deriving instance MonadWriter w m => MonadWriter w (DepT e_ m)

deriving instance MonadError e m => MonadError e (DepT e_ m)

-- | 'DepT' can be d-lifted to a 'ReaderT' in which the environment record
-- containing further 'DepT' actions has been hidden behind a newtype. 
--
-- This can be useful to "deceive" a function into using an environment
-- possessing different instances than the instances seen by the function's
-- dependencies.
instance (Monad m, Coercible newtyped (e_ (DepT e_ m))) => LiftDep (DepT e_ m) (ReaderT newtyped m) where
  liftD = coerce


-- | 'DepT' can be d-lifted to itself.
instance Monad m => LiftDep (DepT e_ m) (DepT e_ m) where
  liftD = id

-- |
--    Runs a 'DepT' action in an environment.
--      
-- >>> runDepT (pure "foo") NilEnv
-- "foo"
--
--    For more sophisticated invocation functions, see @runFinalDepT@ and @runFromEnv@ from <http://hackage.haskell.org/package/dep-t-advice dep-t-advice>.
runDepT :: DepT e_ m r -> e_ (DepT e_ m) -> m r
runDepT = runReaderT . toReaderT

-- |
--    Changes the environment of a 'DepT', for example making the 'DepT' work in
--    a "bigger" environment than the one in which was defined initially.
--
--    The scary first parameter is a function that, given a natural
--    transformation of monads, changes the monad parameter of the environment
--    record. This function can be defined manually for each environment record,
--    or it can be generated using TH from the <http://hackage.haskell.org/package/rank2classes-1.4.1/docs/Rank2-TH.html#v:deriveFunctor rank2classes> package.
withDepT ::
  forall small big m a.
  Monad m =>
  -- | rank-2 map function
  ( forall p q.
    (forall x. p x -> q x) ->
    small p ->
    small q
  ) ->
  -- | get a small environment from a big one
  (forall t. big t -> small t) ->
  DepT small m a ->
  DepT big m a
withDepT mapEnv inner (DepT (ReaderT f)) =
  DepT
    ( ReaderT
        ( \big ->
            let small :: small (DepT small m)
                -- we have a big environment at hand, so let's extract the
                -- small environment, transform every function in the small
                -- environment by supplying the big environment and, as a
                -- finishing touch, lift from the base monad m so that it
                -- matches the monad expected by f.
                small = mapEnv (lift . flip runDepT big) (inner big)
             in f small
        )
    )

-- |
--    Makes the functions inside a small environment require a bigger environment.
--
--    The scary first parameter is a function that, given a natural
--    transformation of monads, changes the monad parameter of the environment
--    record. This function can be defined manually for each environment record,
--    or it can be generated using TH from the <http://hackage.haskell.org/package/rank2classes-1.4.1/docs/Rank2-TH.html#v:deriveFunctor rank2classes> package.
--
--    'zoomEnv' can be useful if we are encasing some preexisting small environment as a field of
--    a big environment, in order to make the types match:
--
-- >>> :{ 
--   type Env :: (Type -> Type) -> Type
--   data Env m = Env
--     { _logger :: String -> m (),
--       _repository :: Int -> m (),
--       _controller :: Int -> m String
--     }
--   $(Rank2.TH.deriveFunctor ''Env)
--   env :: Env (DepT Env IO)
--   env = Env 
--     { _logger = \_ -> pure (), 
--       _repository = \_ -> pure (), 
--       _controller = \_ -> pure "foo" 
--     }
--   type BiggerEnv :: (Type -> Type) -> Type
--   data BiggerEnv m = BiggerEnv
--     { _inner :: Env m,
--       _extra :: Int -> m Int
--     }
--   biggerEnv :: BiggerEnv (DepT BiggerEnv IO)
--   biggerEnv = BiggerEnv 
--     { _inner = zoomEnv (Rank2.<$>) _inner env, 
--       _extra = pure
--     }
-- :}
--
-- __However__, this is only needed when the monad of the smaller environment
-- is already \"fixed\" before inserting it in the bigger one—which I expect
-- to be an infrequent case. When the concrete monad is selected after nesting
-- the environments, 'zoomEnv' shouldn't be necessary.
--
{-# NOINLINE zoomEnv #-}
-- For the reason for not inlining, see https://twitter.com/DiazCarrete/status/1350116413445439493
zoomEnv ::
  forall small big m a.
  Monad m =>
  -- | rank-2 map function
  ( forall p q.
    (forall x. p x -> q x) ->
    small p ->
    small q
  ) ->
  -- | get a small environment from a big one
  (forall t. big t -> small t) ->
  small (DepT small m) ->
  small (DepT big m)
zoomEnv mapEnv inner = mapEnv (withDepT mapEnv inner)

-- | An empty environment that carries no functions, analogous to `()` for `ReaderT`.
type NilEnv :: (Type -> Type) -> Type
data NilEnv m = NilEnv


-- $constant
--
-- 'Constant', which has a phantom type parameter, is a valid environment for
-- 'DepT'.
-- 
-- @DepT (Constant e) m@ makes 'DepT' behave similarly to @ReaderT e m@,
-- in that the environment @e@ is independent of the monad. 
-- 