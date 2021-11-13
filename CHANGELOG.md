# Revision history for dep-t

## 0.5.0.0

* Now `Has` has `Typeable` requirements.

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
