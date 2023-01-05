# Revision history for dep-t

## 0.6.5

* Shifted `DepT`-specific parts of the readme to `Control.Monad.Dep`.

## 0.6.4

* Added AccumConstructor, a generalization of Constructor that threads a
  monoidal summary along with the environment record.

* Now Dep.Env.Phased.traverseH and related functions provide a 'Typeable'
  constraint on the polymorphic field variable in the function argument.

  This *should* be backwards compatible because, with the 'Typeable' constraint,
  the HOF is providing *more* info to the caller of 'traverseH' to work with.

  However it *does* cause a little breakage, because now you can't use FunctorT,
  TraversableT and ApplicativeT from the "barbies" library to implement Phased.
  But most of the time you would use Phased's own generic deriving anyway.

## 0.6.3

* Now the Bare typeclass doesn't recurse inside Identity or Const.

  Strictly speaking, this is a breaking change, but the cases in which it breaks
  are very unlikely in practice: when using Identity or Const in the *middle* of
  a sequence of phases. Typically, these two applicatives will be used as the
  colophon of the sequence of phases.
  
  The motivation for the change is that letting Bare "peek" inside Identity and
  Const made it difficult to write fromBare-using functions polymorphic on the
  returned component. Bare got stuck on the component type variable and the
  functions did not compile. This led to unnecessary code duplication. Stopping
  at Identity means Bare never touches the type variable and doesn't get stuck.

## 0.6.2

* Moved `fromBare` and `toBare` from dep-t-dynamic.

* Doc fix by @eyeinsky (PR #20)

## 0.6.1.0

* Re-export `Data.Functor.Constant` from `Control.Monad.Dep`. https://github.com/danidiaz/dep-t/issues/18

## 0.6.0.0

* Added module `Dep.Tagged`.

* Changed the `Constructor` type synonym. 

  Now it takes a fully constructed environment type. 

  This is a backwards-incompatible change. Type signatures might need to be modified, not so much term level code.

  https://github.com/danidiaz/dep-t/issues/17

* Removed deprecated modules.

## 0.5.1.0

* `Control.Monad.Dep.Has` and `Control.Monad.Dep.Env` renamed as `Dep.Has` and `Dep.Env`.

  The old modules still remain, but deprecated.

## 0.5.0.0

* `Phased` now has `Typeable` constraints. Should be a mostly backwards compatible
  change, as `Typeable` instances are automagically generated for most types.

  Motivated by https://github.com/danidiaz/dep-t-dynamic/issues/1

## 0.4.6.0

* added new module Control.Monad.Dep.Env with helpers for defining environments of records.

## 0.4.5.0

* added "asCall" to Control.Monad.Dep.Has

## 0.4.4.0

* added Control.Monad.Dep.Has, a generic "Has" typeclass which favors a style in which
  the components come wrapped in records or newtypes.

* added "useEnv" to Control.Monad.Dep.Class.

## 0.4.0.0

Actually no breaking changes here, but a change in the recommended structure of
the HasX helper classes, and in how to write general code against those
typeclasses.

* added Control.Monad.Dep.Class

## 0.1.3.0

* re-exported Control.Monad.Trans

## 0.1.2.0

* re-exported Control.Monad.Reader.Class

## 0.1.1.0

* Added NilEnv.

## 0.1.0.2 

* Minor documentation changes.

## 0.1.0.1 

* Minor documentation changes.

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
