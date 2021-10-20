# dep-t

`DepT` is a
[ReaderT](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html)-like
monad transformer for dependency injection.

The difference with `ReaderT` is that `DepT` takes an enviroment whose type is
parameterized by `DepT` itself.

## Rationale

To perform dependency injection in Haskell, a common solution is to build a
record of functions and pass it to the program logic using some variant of
[`ReaderT`](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html).

To avoid becoming tied to a concrete reader environment, let's define some
auxiliary typeclasses that extract functions from a generic environment:

    type HasLogger :: (Type -> Type) -> Type -> Constraint
    class HasLogger d e | e -> d where
      logger :: e -> String -> d ()

    type HasRepository :: (Type -> Type) -> Type -> Constraint
    class HasRepository d e | e -> d where
      repository :: e -> Int -> d ()

We see that the type `e` of the environment determines the monad `d` on which
the effects take place.

Here's a monomorphic environment record with functions that have effects in `IO`:

    type EnvIO :: Type
    data EnvIO = EnvIO
      { _loggerIO :: String -> IO (),
        _repositoryIO :: Int -> IO ()
      }

    instance HasLogger IO EnvIO where
      logger = _loggerIO

    instance HasRepository IO EnvIO where
      repository = _repositoryIO

[Record-of-functions-in-IO](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) is a simple technique which works well in many
situations. There are even [specialized
libraries](http://hackage.haskell.org/package/rio) that support it.

Here's a function which can get its dependencies from the monomorphic
environment:

    mkControllerIO :: (HasLogger IO e, HasRepository IO e) => Int -> ReaderT e IO String
    mkControllerIO x = do
      e <- ask
      liftIO $ logger e "I'm going to insert in the db!"
      liftIO $ repository e x
      return "view"

That's all and well, but there are two issues that bug me:

- We might want to write code that is innocent of `IO` and polymorphic over the
  monad, to ensure that the program logic can't do some unexpected missile
  launch, or to allow testing our app in a "pure" way. 

- What if the repository function needs access to the logger, too? The
  repository lives in the environment record, but isn't aware of it. That means
  it can't use the `HasLogger` typeclass for easy and convenient dependency
  injection. Why privilege the controller in such a way?

  In a sufficiently complex app, the diverse functions that comprise it will be
  organized in a big
  [DAG](https://en.wikipedia.org/wiki/Directed_acyclic_graph) of dependencies.
  And it would be nice if all the functions taking part in dependency injection
  were treated uniformly; if all of them had access to (some view of) the
  environment record.

To tackle these issues, we begin by giving the controller a more general signature:

    mkControllerIO :: (HasLogger IO e, HasRepository IO e, MonadIO m, MonadReader e m) => Int -> m String

Now the function can work in other reader-like monads besides `ReaderT`.

Let's go one step further, and abstract away the `IO`, so that functions in the
record can have effects in other monads:

    mkController :: (HasLogger d e, HasRepository d e, LiftDep d m, MonadReader e m) => Int -> m String
    mkController x = do
      e <- ask
      liftD $ logger e "I'm going to insert in the db!"
      liftD $ repository e x
      return "view"

Now both the signature and the implementation have changed:

- There's a new type variable `d`, the monad in which functions taken from the
  environment `e` have their effects.

- `MonadIO` has been replaced by `LiftDep` from `Control.Monad.Dep.Class`, a
  constraint that says we can lift `d` effects into `m` (though it could still
  make sense to require `MonadIO m` for effects not originating in the
  environment).

- Uses of `liftIO` have been replaced by `liftD`.

If all those constraints prove annoying to write, there's a convenient shorthand using the `MonadDep` type family:

    mkController :: MonadDep [HasLogger, HasRepository] d e m => Int -> m String

The new, more polymorphic `mkController` function can replace the original `mkControllerIO`:

    mkControllerIO' :: (HasLogger IO e, HasRepository IO e) => Int -> ReaderT e IO String
    mkControllerIO' = mkController

Now let's focus on the environment record. We'll parameterize its type by a
monad: 

    type Env :: (Type -> Type) -> Type
    data Env m = Env
      { _logger :: String -> m (),
        _repository :: Int -> m (),
        _controller :: Int -> m String
      }

    instance HasLogger m (Env m) where
      logger = _logger

    instance HasRepository m (Env m) where
      repository = _repository

Notice that the controller function is now part of the environment. No
favorites here!

The following implementation of the logger function has no dependencies besides
`MonadIO`:

    mkStdoutLogger :: MonadIO m => String -> m ()
    mkStdoutLogger msg = liftIO (putStrLn msg)

But look at this implementation of the repository function. It gets hold of the
logger through `HasLogger`, just as the controller did:

    mkStdoutRepository :: (MonadDep '[HasLogger] d e m, MonadIO m) => Int -> m ()
    mkStdoutRepository entity = do
      e <- ask
      liftD $ logger e "I'm going to write the entity!"
      liftIO $ print entity

It's about time we choose a concrete monad and assemble an environment record:

    envIO :: Env (DepT Env IO)
    envIO =
      let _logger = mkStdoutLogger
          _repository = mkStdoutRepository
          _controller = mkController
       in Env {_logger,  _repository, _controller}

Not very complicated, except... what is that weird `DepT Env IO` doing there in
the signature? 

Well, that's the whole reason this library exists. For dependency injection to
work for *all* functions, `Env` needs to be parameterized with a monad that
provides that same `Env` environment. And trying to use a `ReaderT (Env
something) IO` to parameterize `Env` won't fly; you'll get weird "infinite
type" kind of errors. So I created the `DepT` newtype over `ReaderT` to mollify
the compiler.

`DepT` has `MonadReader` and `LiftDep` instances, so the effects of
`mkController` can take place on it.

## Inter-module dependencies

[![dep-t.png](https://i.postimg.cc/KcwgCBWv/dep-t.png)](https://postimg.cc/bd0JtD8K)

- __Control.Monad.Dep.Class__ can be used to program against both `ReaderT` and `DepT`.
- __Control.Monad.Dep__ contains the actual `DepT` monad transformer.
- __Control.Monad.Dep.Has__ can be useful independently of `ReaderT`, `DepT` or any monad transformer.
- __Control.Monad.Dep.Env__ provides extra definitions that help when building environments of records.

## So how do we invoke the controller now?

I suggest something like

    runDepT (do e <- ask; _controller e 7) envIO 

or 

    (do e <- ask; _controller e 7) `runDepT` envIO 

The companion package
[dep-t-advice](http://hackage.haskell.org/package/dep-t-advice) has some more
functions for running `DepT` computations.

## How to avoid using "ask" and "liftD" before invoking a dependency?

One possible workaround (at the cost of more boilerplate) is to define helper
functions like:  

    loggerD :: MonadDep '[HasLogger] d e m => String -> m ()
    loggerD msg = asks logger >>= \f -> liftD $ f msg

Which you can invoke like this:

    usesLoggerD :: MonadDep [HasLogger, HasRepository] d e m => Int -> m String
    usesLoggerD i = do
      loggerD "I'm calling the logger!"
      return "foo"

Though perhaps this isn't worth the hassle.

## How to use "pure fakes" during testing?

The [test suite](./test/tests.hs) has an example of using a `Writer` monad for
collecting the outputs of functions working as ["test
doubles"](https://martinfowler.com/bliki/TestDouble.html).

## How to make a function "see" a different evironment from the one seen by its dependencies?

Sometimes we want a function in the environment to see a slightly different
record from the record seen by the other functions, and in particular from the
record seen by its own dependencies. 

For example, the function might have a `HasLogger` constraint but we don't want
it to use the default `HasLogger` instance of the environment.

The companion package
[dep-t-advice](http://hackage.haskell.org/package/dep-t-advice) provides a
`deceive` function that allows for this.

## How to add AOP-ish "aspects" to functions in an environment?

The companion package
[dep-t-advice](http://hackage.haskell.org/package/dep-t-advice) provides a
general method of extending the behaviour of `DepT`-effectful functions, in a
way reminiscent of aspect-oriented programming.

## What if I don't want to use DepT, or any other monad transformer for that matter?

Check out the function `fixEnv` in module `Control.Monad.Dep.Env`, which
provides a transformer-less way to perform dependency injection, based on
knot-tying.

That method requires an environment parameterized by _two_ type constructors:
one that wraps each field, and another that works as the effect monad for the
components.

## Caveats

The structure of the `DepT` type might be prone to trigger a [known infelicity
of the GHC
simplifier](https://twitter.com/DiazCarrete/status/1350116413445439493).

## Links

- This library was extracted from my answer to [this Stack Overflow
  question](https://stackoverflow.com/a/61782258/1364288).

- The implementation of `mapDepT` was teased out in [this other SO question](https://stackoverflow.com/questions/65710657/writing-a-zooming-function-for-a-readert-like-monad-transformer).

- An [SO
  answer](https://stackoverflow.com/questions/57703898/how-to-call-impure-functions-from-pure-ones/57714058#57714058)
  about records-of-functions and the "veil of polymorphism".

- The answers to [this SO
  question](https://stackoverflow.com/questions/61642492/simplifying-the-invocation-of-functions-stored-inside-an-readert-environment)
  gave me the idea for how to "instrument" monadic functions (although the
  original motive of the question was different).

- I'm unsure of the relationship between `DepT` and the technique described in
  [Adventures assembling records of
  capabilities](https://discourse.haskell.org/t/adventures-assembling-records-of-capabilities/623)
  which relies on having "open" and "closed" versions of the environment
  record, and getting the latter from the former by means of knot-tying. 

  It seems that, with `DepT`, functions in the environment obtain their
  dependencies anew every time they are invoked. If we change a function in the
  environment record, all other functions which depend on it will be affected
  in subsequent invocations. I don't think this happens with "Adventures..." at
  least when changing a "closed", already assembled record.

  With `DepT` a function might use `local` if it knows enough about the
  environment. That doesn't seem very useful for program logic; if fact it
  sounds like a recipe for confusion. It could perhaps be useful for [AOP-ish
  things](http://hackage.haskell.org/package/dep-t-advice), to keep a synthetic
  "call stack", or to implement something like Logback's [Mapped Diagnostic
  Context](http://logback.qos.ch/manual/mdc.html).

  So perhaps `DepT` will be overkill in a lot of cases, offering unneeded
  flexibility. Perhaps using `fixEnv` from `Control.Monad.Dep.Env` will end up
  being simpler.

  Unlike in "Adventures..." the `fixEnv` method doesn't use an extensible
  record for the environment but, to keep things simple, a suitably
  parameterized conventional one.

- Another exploration of dependency injection with `ReaderT`:
  [ReaderT-OpenProduct-Environment](https://github.com/keksnicoh/ReaderT-OpenProduct-Environment).

- [The ReaderT design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/).

  > Your application code will, in general, live in ReaderT Env IO. Define it as type App = ReaderT Env IO if you wish, or use a newtype wrapper instead of ReaderT directly.

  > Optional: instead of directly using the App datatype, write your functions in terms of mtl-style typeclasses like MonadReader and MonadIO

- [RIO](http://hackage.haskell.org/package/rio) is a featureful ReaderT-like /
  prelude replacement library which favors monomorphic environments.

- The [van Laarhoven Free Monad](http://r6.ca/blog/20140210T181244Z.html).

  > Swierstra notes that by summing together functors representing primitive I/O
  > actions and taking the free monad of that sum, we can produce values use
  > multiple I/O feature sets. Values defined on a subset of features can be
  > lifted into the free monad generated by the sum. The equivalent process can
  > be performed with the van Laarhoven free monad by taking the product of
  > records of the primitive operations. Values defined on a subset of features
  > can be lifted by composing the van Laarhoven free monad with suitable
  > projection functions that pick out the requisite primitive operations. 

  [Another post](https://www.tweag.io/blog/2019-03-20-capability-free-monad/van) about the van Laarhoven Free Monad. Is it related to the final encoding of Free monads described [here](https://blog.poisson.chat/posts/2021-10-20-initial-final-free-monad.html)?

- [Interesting SO response](https://stackoverflow.com/a/634754/1364288) (from
  2009) about the benefits of autowiring in Spring. The record-of-functions
  approach in Haskell can't be said to provide true autowiring. You still need
  to assemble the record manually, and field names in the record play the part
  of Spring bean names. 

  > Right now I think the most important reason for using autowiring is that
  > there's one less abstraction in your system to keep track of. The "bean name"
  > is effectively gone. It turns out the bean name only exists because of xml. So
  > a full layer of abstract indirections (where you would wire bean-name "foo"
  > into bean "bar") is gone

- [registry](http://hackage.haskell.org/package/registry) is a package that
  implements an alternative approach to dependency injection, one different
  from the `ReaderT`-based one. 

- [Printf("%s %s", dependency, injection)](https://www.fredrikholmqvist.com/posts/print-dependency-injection/). Commented on [HN](https://news.ycombinator.com/item?id=28915630), [Lobsters](https://lobste.rs/s/4axrt6/printf_s_s_dependency_injection).

- [This book](https://www.goodreads.com/book/show/44416307-dependency-injection-principles-practices-and-patterns) about DI is good.

