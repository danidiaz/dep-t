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
-- This module provides 'DepT', a monad transformer similar to 'ReaderT'.
--
-- The difference with 'ReaderT' is that 'DepT' takes an enviroment whose type is
-- parameterized by 'DepT' itself.
module Control.Monad.Dep
  ( 
    -- * Motivation 
    -- $motivation

    -- ** Caveats
    -- $caveats
    
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

-- $motivation
--
-- Dependency injection.
--
-- To perform dependency injection in Haskell, a common solution is to build a
-- record of functions and pass it to the program logic using some variant of
-- 'ReaderT'.
-- 
-- To avoid becoming tied to a concrete reader environment, let's define some
-- auxiliary typeclasses that extract functions from a generic environment:
--
-- @
-- type HasLogger :: (Type -> Type) -> Type -> Constraint
-- class HasLogger d e | e -> d where
--   logger :: e -> String -> d ()
-- 
-- type HasRepository :: (Type -> Type) -> Type -> Constraint
-- class HasRepository d e | e -> d where
--   repository :: e -> Int -> d ()
-- @
-- 
-- We see that the type `e` of the environment determines the monad `d` on which
-- the effects take place.
-- 
-- Here's a monomorphic environment record with functions that have effects in `IO`:
-- 
-- @
-- type EnvIO :: Type
-- data EnvIO = EnvIO
--   { _loggerIO :: String -> IO (),
--     _repositoryIO :: Int -> IO ()
--   }
-- 
-- instance HasLogger IO EnvIO where
--   logger = _loggerIO
-- 
-- instance HasRepository IO EnvIO where
--   repository = _repositoryIO
-- @
-- 
-- [Record-of-functions-in-IO](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) is a simple technique which works well in many
-- situations. There are even [specialized
-- libraries](http://hackage.haskell.org/package/rio) that support it.
-- 
-- Here's a function which can get its dependencies from the monomorphic
-- environment:
-- 
-- @
-- mkControllerIO :: (HasLogger IO e, HasRepository IO e) => Int -> ReaderT e IO String
-- mkControllerIO x = do
--   e <- ask
--   liftIO $ logger e "I'm going to insert in the db!"
--   liftIO $ repository e x
--   return "view"
-- @
-- 
-- That's all and well, but there are two issues that bug me:
-- 
-- - We might want to write code that is innocent of `IO` and polymorphic over the
--   monad, to ensure that the program logic can't do some unexpected missile
--   launch, or to allow testing our app in a "pure" way. 
-- 
-- - What if the repository function needs access to the logger, too? The
--   repository lives in the environment record, but isn't aware of it. That means
--   it can't use the `HasLogger` typeclass for easy and convenient dependency
--   injection. Why privilege the controller in such a way?
-- 
--   In a sufficiently complex app, the diverse functions that comprise it will be
--   organized in a big
--   [DAG](https://en.wikipedia.org/wiki/Directed_acyclic_graph) of dependencies.
--   And it would be nice if all the functions taking part in dependency injection
--   were treated uniformly; if all of them had access to (some view of) the
--   environment record.
-- 
-- To tackle these issues, we begin by giving the controller a more general signature:
-- 
-- @
-- mkControllerIO :: (HasLogger IO e, HasRepository IO e, MonadIO m, MonadReader e m) => Int -> m String
-- @
-- 
-- Now the function can work in other reader-like monads besides 'ReaderT'.
-- 
-- Let's go one step further, and abstract away the `IO`, so that functions in the
-- record can have effects in other monads:
-- 
-- @
-- mkController :: (HasLogger d e, HasRepository d e, LiftDep d m, MonadReader e m) => Int -> m String
-- mkController x = do
--   e <- ask
--   liftD $ logger e "I'm going to insert in the db!"
--   liftD $ repository e x
--   return "view"
-- @
-- 
-- Now both the signature and the implementation have changed:
-- 
-- - There's a new type variable `d`, the monad in which functions taken from the
--   environment `e` have their effects.
-- 
-- - `MonadIO` has been replaced by `LiftDep` from `Control.Monad.Dep.Class`, a
--   constraint that says we can lift `d` effects into `m` (though it could still
--   make sense to require `MonadIO m` for effects not originating in the
--   environment).
-- 
-- - Uses of `liftIO` have been replaced by `liftD`.
-- 
-- If all those constraints prove annoying to write, there's a convenient shorthand using the `MonadDep` type family:
-- 
-- @
-- MonadDep [HasLogger, HasRepository] d e m => Int -> m String
-- @
-- 
-- The new, more polymorphic `mkController` function can replace the original `mkControllerIO`:
-- 
-- @
-- mkControllerIO' :: (HasLogger IO e, HasRepository IO e) => Int -> ReaderT e IO String
-- mkControllerIO' = mkController
-- @
-- 
-- Now let's focus on the environment record. We'll parameterize its type by a
-- monad: 
-- 
-- @
-- type Env :: (Type -> Type) -> Type
-- data Env m = Env
--   { _logger :: String -> m (),
--     _repository :: Int -> m (),
--     _controller :: Int -> m String
--   }
-- 
-- instance HasLogger m (Env m) where
--   logger = _logger
-- 
-- instance HasRepository m (Env m) where
--   repository = _repository
-- @
-- 
-- Notice that the controller function is now part of the environment. No
-- favorites here!
-- 
-- The following implementation of the logger function has no dependencies besides
-- 'MonadIO':
-- 
-- @
-- mkStdoutLogger :: MonadIO m => String -> m ()
-- mkStdoutLogger msg = liftIO (putStrLn msg)
-- @
-- 
-- But look at this implementation of the repository function. It gets hold of the
-- logger through @HasLogger@, just as the controller did:
-- 
-- @
-- mkStdoutRepository :: (MonadDep '[HasLogger] d e m, MonadIO m) => Int -> m ()
-- mkStdoutRepository entity = do
--   e <- ask
--   liftD $ logger e "I'm going to write the entity!"
--   liftIO $ print entity
-- @
-- 
-- It's about time we choose a concrete monad and assemble an environment record:
-- 
-- @
-- envIO :: Env (DepT Env IO)
-- envIO =
--   let _logger = mkStdoutLogger
--       _repository = mkStdoutRepository
--       _controller = mkController
--    in Env {_logger,  _repository, _controller}
-- @
-- 
-- Not very complicated, except... what is that weird `DepT Env IO` doing there in
-- the signature? 
-- 
-- Well, that's the whole reason this library exists. For dependency injection to
-- work for *all* functions, @Env@ needs to be parameterized with a monad that
-- provides that same @Env@ environment. And trying to use a @ReaderT (Env
-- something) IO@ to parameterize @Env@ won't fly; you'll get weird "infinite
-- type" kind of errors. So I created the 'DepT' newtype over 'ReaderT' to mollify
-- the compiler.
-- 
-- 'DepT' has 'MonadReader' and 'LiftDep' instances, so the effects of
-- @mkController@ can take place on it.
--
-- To invoke the controller from the environment, we can do something like
-- 
-- @
-- runDepT (do e <- ask; _controller e 7) envIO 
-- @
-- 
-- or 
-- 
-- @
-- (do e <- ask; _controller e 7) `runDepT` envIO 
-- @
-- 
-- The companion package
-- [dep-t-advice](http://hackage.haskell.org/package/dep-t-advice) has some
-- helper functions for running 'DepT' computations.
 
-- $caveats
-- The structure of the 'DepT' type might be prone to trigger a [known infelicity
-- of the GHC
-- simplifier](https://twitter.com/DiazCarrete/status/1350116413445439493).


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
--    The environment type must be parameterized with the transformer stack.
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
-- Analogous to 'withReaderT'.
--
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