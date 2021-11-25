{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExplicitForAll #-}

-- | Companion module to "Dep.Has" for disambiguanting record components withing an environment.
--
-- Similar in purpose to the [Qualifier annotation](https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/beans/factory/annotation/Qualifier.html) in Java Spring.
--
-- When using functions from "Dep.SimpleAdvice" (which tend to depend on coercions) with 'Tagged' components, remember to import the newtype's constructor.
module Dep.Tagged
  (
    Tagged (..),
    tagged,
    untag
  )
where

import Data.Kind
import Data.Typeable
import GHC.Generics qualified as G

-- | Very similar to the @Data.Tagged@ type from the \"tagged\" package, but
-- with an extra monad type argument.  The intended use is to disambiguate
-- record components within an environment, when there are multiple records of the same
-- type.
type Tagged :: k -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
newtype Tagged s r_ m = Tagged {unTagged :: r_ m}
  deriving
    ( 
      G.Generic,
      Typeable
    )

-- When inserting into an environment a component that you want to disambiguate, provide the the tag using a type variable and then supply the record value. 
tagged :: forall s r_ m . r_ m -> Tagged s r_ m
tagged = Tagged

-- | Alias for 'unTagged'.
--
-- When invoking a method from a tagged dependency,
-- provide the tag using a type application and compose with the record selector.
untag :: Tagged s r_ m -> r_ m
untag = unTagged

