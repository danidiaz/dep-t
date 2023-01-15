{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Dep.Phases (
    -- * Managing phases
    Phased (..),
    liftAH,
    pullPhase,
    mapPhase,
    liftA2Phase,
    -- * Qualified do-notation for building phases
    -- $warning
    (>>=), 
    (>>),
    -- * Re-exports
    Identity (..),
    Compose (..),
    ) where


import Control.Applicative
import Data.Coerce
import Data.Function (fix)
import Data.Functor (($>), (<&>))
import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Data.String
import Data.Type.Equality (type (==))
import Data.Typeable
import GHC.Generics qualified as G
import GHC.Records
import GHC.TypeLits
import Data.Functor.Compose
import Prelude (Functor, (<$>), (<$), ($), (.))

--
--
-- Managing Phases

-- see also https://github.com/haskell/cabal/issues/7394#issuecomment-861767980

-- | Class of 2-parameter environments for which the first parameter @h@ wraps
-- each field and corresponds to phases in the construction of the environment,
-- and the second parameter @m@ is the effect monad used by each component.
--
-- @h@ will typically be a composition of applicative functors, each one
-- representing a phase. We advance through the phases by \"pulling out\" the
-- outermost phase and running it in some way, until we are are left with a
-- 'Constructor' phase, which we can remove using 'fixEnv'.
--
-- 'Phased' resembles [FunctorT, TraversableT and
-- ApplicativeT](https://hackage.haskell.org/package/barbies-2.0.3.0/docs/Data-Functor-Transformer.html)
-- from the [barbies](https://hackage.haskell.org/package/barbies) library,
-- although 'Phased' instances /can't/ be written in terms of them because of the extra 'Typeable' constraints.
type Phased :: ((Type -> Type) -> (Type -> Type) -> Type) -> Constraint
class Phased (env_ :: (Type -> Type) -> (Type -> Type) -> Type) where
  -- | Used to implement 'pullPhase' and 'mapPhase',  typically you should use those functions instead.
  traverseH ::
    forall
      (h :: Type -> Type)
      (f :: Type -> Type)
      (g :: Type -> Type)
      (m :: Type -> Type).
    ( 
      Applicative f,
      Typeable f,
      Typeable g,
      Typeable h,
      Typeable m
    ) =>
    -- | Transform to be applied to each field.
    (forall x. Typeable x => h x -> f (g x)) ->
    env_ h m ->
    f (env_ g m)
  default traverseH ::
    forall
      (h :: Type -> Type)
      (f :: Type -> Type)
      (g :: Type -> Type)
      (m :: Type -> Type).
    ( Applicative f,
      Typeable f,
      Typeable g,
      Typeable h,
      Typeable m,
      G.Generic (env_ h m),
      G.Generic (env_ g m),
      GTraverseH h g (G.Rep (env_ h m)) (G.Rep (env_ g m))
    ) =>
    -- | Transform to be applied to each field.
    (forall x. Typeable x => h x -> f (g x)) ->
    env_ h m ->
    f (env_ g m)
  traverseH t env = G.to <$> gTraverseH t (G.from env)

  -- | Used to implement 'liftA2Phase', typically you should use that function instead.
  liftA2H ::
    forall (a :: Type -> Type) (f :: Type -> Type) (f' :: Type -> Type) (m :: Type -> Type).
    ( Typeable a,
      Typeable f,
      Typeable f',
      Typeable m
    ) =>
    -- | Binary operation to combine corresponding fields.
    (forall x. Typeable x => a x -> f x -> f' x) ->
    env_ a m ->
    env_ f m ->
    env_ f' m
  default liftA2H ::
    forall (a :: Type -> Type) (f :: Type -> Type) (f' :: Type -> Type) m.
    ( Typeable a,
      Typeable f,
      Typeable f',
      Typeable m,
      G.Generic (env_ a m),
      G.Generic (env_ f m),
      G.Generic (env_ f' m),
      GLiftA2Phase a f f' (G.Rep (env_ a m)) (G.Rep (env_ f m)) (G.Rep (env_ f' m))
    ) =>
    -- | Transform to be applied to each field.
    (forall x. Typeable x => a x -> f x -> f' x) ->
    env_ a m ->
    env_ f m ->
    env_ f' m
  liftA2H f enva env = G.to (gLiftA2Phase f (G.from enva) (G.from env))

-- | Slightly less powerful version of 'traverseH'.
liftAH ::
  forall deps_ phases phases' m.
  (Phased deps_, Typeable phases, Typeable phases', Typeable m) =>
  -- | Function that changes the phase wrapping each component.
  (forall x. Typeable x => phases x -> phases' x) ->
  deps_ phases m ->
  deps_ phases' m
liftAH tweak =
  runIdentity . traverseH (Identity . tweak)

-- | Take the outermost phase wrapping each component and \"pull it outwards\",
-- aggregating the phase's applicative effects.
--
-- >>> :{
--  newtype Foo d = Foo {foo :: String -> d ()} deriving Generic
--  makeIOFoo :: MonadIO m => Foo m
--  makeIOFoo = Foo (liftIO . putStrLn)
--  env :: InductiveEnv '[Foo] (IO `Compose` Constructor (InductiveEnv '[Foo] Identity IO)) IO
--  env = EmptyEnv
--      & AddDep @Foo (putStrLn "io phase" `bindPhase` \() -> constructor (\_ -> makeIOFoo))
--  ioOutside :: IO (InductiveEnv '[Foo] (Constructor (InductiveEnv '[Foo] Identity IO)) IO)
--  ioOutside = pullPhase env
-- :}
pullPhase ::
  forall (f :: Type -> Type) (g :: Type -> Type) (m :: Type -> Type) env_.
  (Phased env_, Applicative f, Typeable f, Typeable g, Typeable m) =>
  env_ (Compose f g) m ->
  -- | Environment with the outer 'Applicative' layer pulled outward.
  f (env_ g m)
-- f first to help annotate the phase
pullPhase = traverseH @env_ getCompose

-- | Modify the outermost phase wrapping each component.
--
-- >>> :{
--  newtype Foo d = Foo {foo :: String -> d ()} deriving Generic
--  makeIOFoo :: MonadIO m => Foo m
--  makeIOFoo = Foo (liftIO . putStrLn)
--  env :: InductiveEnv '[Foo] ((,) Int `Compose` Constructor String) IO
--  env = EmptyEnv
--      & AddDep @Foo ((2,()) `bindPhase` \() -> constructor (\_ -> makeIOFoo))
--  env' :: InductiveEnv '[Foo] ((,) String `Compose` Constructor String) IO
--  env' = mapPhase (\(n,x) -> (show n,x)) env
-- :}
mapPhase ::
  forall (f :: Type -> Type) (f' :: Type -> Type) (g :: Type -> Type) (m :: Type -> Type) env_.
  (Phased env_, Typeable f, Typeable f', Typeable g, Typeable m) =>
  -- | Transform to be applied to each field.
  (forall x. Typeable x => f x -> f' x) ->
  env_ (Compose f g) m ->
  env_ (Compose f' g) m
-- f' first to help annotate the *target* of the transform?
mapPhase f env = runIdentity $ traverseH @env_ (\(Compose fg) -> Identity (Compose (f fg))) env

-- | Combine two environments with a function that works on their outermost phases.
liftA2Phase ::
  forall (a :: Type -> Type) (f' :: Type -> Type) (f :: Type -> Type) (g :: Type -> Type) (m :: Type -> Type) env_.
  (Phased env_, Typeable a, Typeable f, Typeable f', Typeable g, Typeable m) =>
  -- | Binary operation to combine corresponding fields.
  (forall x. Typeable x => a x -> f x -> f' x) ->
  env_ (Compose a g) m ->
  env_ (Compose f g) m ->
  env_ (Compose f' g) m
-- f' first to help annotate the *target* of the transform?
liftA2Phase f = liftA2H @env_ (\(Compose fa) (Compose fg) -> Compose (f fa fg))

class GTraverseH h g env env' | env -> h, env' -> g where
  gTraverseH :: Applicative f => (forall x. Typeable x => h x -> f (g x)) -> env x -> f (env' x)

instance
  (GTraverseH h g fields fields') =>
  GTraverseH
    h
    g
    (G.D1 metaData (G.C1 metaCons fields))
    (G.D1 metaData (G.C1 metaCons fields'))
  where
  gTraverseH t (G.M1 (G.M1 fields)) =
    G.M1 . G.M1 <$> gTraverseH @h @g t fields

instance
  ( GTraverseH h g left left',
    GTraverseH h g right right'
  ) =>
  GTraverseH h g (left G.:*: right) (left' G.:*: right')
  where
  gTraverseH t (left G.:*: right) =
    let left' = gTraverseH @h @g t left
        right' = gTraverseH @h @g t right
     in liftA2 (G.:*:) left' right'

instance
  Typeable bean =>
  GTraverseH
    h
    g
    (G.S1 metaSel (G.Rec0 (h bean)))
    (G.S1 metaSel (G.Rec0 (g bean)))
  where
  gTraverseH t (G.M1 (G.K1 hbean)) =
    G.M1 . G.K1 <$> t hbean

--
--
class GLiftA2Phase a f f' enva env env' | enva -> a, env -> f, env' -> f' where
  gLiftA2Phase :: (forall r. Typeable r => a r -> f r -> f' r) -> enva x -> env x -> env' x

instance
  GLiftA2Phase a f f' fieldsa fields fields' =>
  GLiftA2Phase
    a
    f
    f'
    (G.D1 metaData (G.C1 metaCons fieldsa))
    (G.D1 metaData (G.C1 metaCons fields))
    (G.D1 metaData (G.C1 metaCons fields'))
  where
  gLiftA2Phase f (G.M1 (G.M1 fieldsa)) (G.M1 (G.M1 fields)) =
    G.M1 (G.M1 (gLiftA2Phase @a @f @f' f fieldsa fields))

instance
  ( GLiftA2Phase a f f' lefta left left',
    GLiftA2Phase a f f' righta right right'
  ) =>
  GLiftA2Phase a f f' (lefta G.:*: righta) (left G.:*: right) (left' G.:*: right')
  where
  gLiftA2Phase f (lefta G.:*: righta) (left G.:*: right) =
    let left' = gLiftA2Phase @a @f @f' f lefta left
        right' = gLiftA2Phase @a @f @f' f righta right
     in (G.:*:) left' right'

instance
  Typeable bean =>
  GLiftA2Phase
    a
    f
    f'
    (G.S1 metaSel (G.Rec0 (a bean)))
    (G.S1 metaSel (G.Rec0 (f bean)))
    (G.S1 metaSel (G.Rec0 (f' bean)))
  where
  gLiftA2Phase f (G.M1 (G.K1 abean)) (G.M1 (G.K1 fgbean)) =
    G.M1 (G.K1 (f abean fgbean))


-- | Examples without @-XQualifiedDo@:
--
-- >>> :{
--  type Phases = IO `Compose` IO `Compose` Identity
--  phased :: Phases Int
--  phased =
--      pure 1 Dep.Phases.>>= \i1 ->
--      pure 2 Dep.Phases.>>= \i2 ->
--      pure $ i1 + i2
-- :}
--
--
-- >>> :{
-- type Phases = (IO `Compose` Maybe `Compose` Either Char) Int
-- phases :: Phases
-- phases = 
--    pure () Dep.Phases.>>= \_ ->
--    Just 5 Dep.Phases.>>= \_ ->
--    Left 'e'
-- :}
--
--
(>>=) :: Functor f => f x -> (x -> g y) -> Compose f g y
f >>= k = Compose (k <$> f)

-- | Better not use this one without @-XQualifiedDo@
(>>) :: Functor f => f x -> g y -> Compose f g y
f >> g = Compose (g <$ f)

-- $warning
-- Convenient [qualified
-- do-notation](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/qualified_do.html#extension-QualifiedDo)
-- for defining nested applicative phases wrapped in 'Compose's.
-- 
-- __BEWARE__! Despite its convenience, this do-notation lacks [many of the properties](https://wiki.haskell.org/Monad_laws#The_monad_laws_in_practice) 
-- we tend to assume when working with do-notation. In particular, it's 
-- NOT associative! This means that if we have 
--
-- @
-- Dep.Phases.do    
--    somePhase
--    someOtherPhase
--    finalPhase
-- @
--
-- we CAN'T refactor to
--
-- @
-- Dep.Phases.do    
--    Dep.Phases.do 
--      somePhase
--      someOtherPhase
--    finalPhase
-- @
--
-- It would indeed be useful (it would allow pre-packaging and sharing initial
-- phases as do-blocks) but it isn't supported.
--
-- __BEWARE__ #2! Do not use 'return' in this do-notation.
--
-- Some valid examples:
--
-- >>> :{
-- type Phases = (IO `Compose` IO `Compose` IO) Int
-- phases :: Phases
-- phases = Dep.Phases.do
--    r1 <- pure 1
--    r2 <- pure 2
--    pure $ r1 + r2
-- :}
--
--
-- >>> :{
-- type Phases = (IO `Compose` Maybe `Compose` Either Char) Int
-- phases :: Phases
-- phases = Dep.Phases.do
--    pure ()
--    Just 5
--    Left 'e'
-- :}
--
--


-- $setup
--
-- >>> :set -XTypeApplications
-- >>> :set -XMultiParamTypeClasses
-- >>> :set -XImportQualifiedPost
-- >>> :set -XStandaloneKindSignatures
-- >>> :set -XNamedFieldPuns
-- >>> :set -XFunctionalDependencies
-- >>> :set -XFlexibleContexts
-- >>> :set -XDataKinds
-- >>> :set -XBlockArguments
-- >>> :set -XFlexibleInstances
-- >>> :set -XTypeFamilies
-- >>> :set -XDeriveGeneric
-- >>> :set -XViewPatterns
-- >>> :set -XDerivingStrategies
-- >>> :set -XDerivingVia
-- >>> :set -XDeriveAnyClass
-- >>> :set -XStandaloneDeriving
-- >>> :set -XUndecidableInstances
-- >>> :set -XTypeOperators
-- >>> :set -XScopedTypeVariables
-- >>> :set -XQualifiedDo
-- >>> :set -fno-warn-deprecations
-- >>> import Data.Kind
-- >>> import Data.Function ((&))
-- >>> import Control.Monad.IO.Class
-- >>> import Dep.Env
-- >>> import GHC.Generics (Generic)
-- >>> import Prelude hiding ((>>=), (>>))
