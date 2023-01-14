module Dep.Phases (
    -- * Qualified do-notation for building phases
    -- $warning
    (>>=), 
    (>>),
    -- * Re-exports
    Compose (..),
    ) where

import Data.Functor.Compose
import Prelude (Functor, (<$>), (<$))

-- | Without @-XQualifiedDo@:
--
-- >>> :{
--  type Phases = IO `Compose` IO `Compose` Identity
--  phased :: Phases Int
--  phased =
--      pure 1 Dep.Phases.>>= \i1 ->
--      pure 2 Dep.Phases.>>= \i2 ->
--      Identity (i1 + i2)
-- :}
--
--
-- With @-XQualifiedDo@:
--
-- >>> :{
--  type Phases = IO `Compose` IO `Compose` Identity
--  phased :: Phases Int
--  phased = Dep.Phases.do
--      i1 <- pure 1
--      i2 <- pure 2
--      Identity (i1 + i2)
-- :}
--
(>>=) :: Functor f => f x -> (x -> g y) -> Compose f g y
f >>= k = Compose (k <$> f)


-- | Without @-XQualifiedDo@:
--
-- >>> :{
--  type Phases = IO `Compose` IO `Compose` Identity
--  phased :: Phases Int
--  phased =
--      pure () Dep.Phases.>>
--      (pure () Dep.Phases.>>
--       Identity 1)
-- :}
--
--
-- With @-XQualifiedDo@:
--
-- >>> :{
--  type Phases = IO `Compose` IO `Compose` Identity
--  phased :: Phases Int
--  phased = Dep.Phases.do
--      pure () 
--      pure () 
--      Identity 1
-- :}
--
--
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
-- >>> import Dep.Env
-- >>> import GHC.Generics (Generic)
-- >>> import Prelude hiding ((>>=), (>>))
