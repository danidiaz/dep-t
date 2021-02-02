# dep-t

`DepT` is a
[ReaderT](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html)-like
monad transformer for dependency injection.

The difference with `ReaderT` is that `DepT` takes an enviroment whose type is
parameterized by `DepT` itself.

## Rationale

To achieve dependency injection in Haskell, a common solution is to build a
record of functions and pass it to the program logic using some variant of
[`ReaderT`](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html).

Let's start by defining some auxiliary typeclasses to extract functions from an
environment record:

    type HasLogger :: (Type -> Type) -> Type -> Constraint
    class HasLogger d e | e -> d where
      logger :: e -> String -> d ()

    type HasRepository :: (Type -> Type) -> Type -> Constraint
    class HasRepository d e | e -> d where
      repository :: e -> Int -> d ()

We see that the type of the record determines the monad in which the effects take place.

Let's define a monomorphic record with effects in `IO`:

    type EnvIO :: Type
    data EnvIO = EnvIO
      { _loggerIO :: String -> IO (),
        _repositoryIO :: Int -> IO ()
      }

    instance HasLogger IO EnvIO where
      logger = _loggerIO

    instance HasRepository IO EnvIO where
      repository = _repositoryIO

Record-of-functions-in-IO is a simple technique which works well in many
situations. There are even [specialized
libraries](http://hackage.haskell.org/package/rio) that support it.

Here's a function which obtains its dependencies from the environment record:

    mkControllerIO :: (HasLogger e IO, HasRepository e IO) => Int -> ReaderT e IO String
    mkControllerIO x = do
      e <- ask
      liftIO $ logger e "I'm going to insert in the db!"
      liftIO $ repository e x
      return "view"

That's all and well, but there are two issues that bug me:

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

- We might want to write code that is innocent of `IO` and polymorphic over the
  monad, to ensure that the program logic can't do some unexpected missile
  launch, or to allow testing our app in a "pure" way. 

Let's parameterize our environment by a monad: 

    type Env :: (Type -> Type) -> Type
    data Env m = Env
      { _logger :: String -> m (),
        _repository :: Int -> m (),
        _controller :: Int -> m String
      }
    -- helper from the "rank2classes" package
    $(Rank2.TH.deriveFunctor ''Env)

    instance HasLogger (Env m) m where
      logger = _logger

    instance HasRepository (Env m) m where
      repository = _repository

Notice that the controller function is now part of the environment. No
favorites here!

The following implementation of the logger function has no dependencies besides
`MonadIO`:

    mkStdoutLogger :: MonadIO m => String -> m ()
    mkStdoutLogger msg = liftIO (putStrLn msg)

But look at this implementation of the repository function. It gets hold of the
logger through `HasLogger`:

    mkStdoutRepository :: (MonadReader e m, HasLogger e m, MonadIO m) => Int -> m ()
    mkStdoutRepository entity = do
      e <- ask
      logger e "I'm going to write the entity!"
      liftIO $ print entity

And here's the controller:

    mkController :: (MonadReader e m, HasLogger e m, HasRepository e m) => Int -> m String
    mkController x = do
      e <- ask
      logger e "I'm going to insert in the db!"
      repository e x
      return "view"

Now, lets choose `IO` as the base monad and assemble an environment record:

    envIO :: Env (DepT Env IO)
    envIO =
      let _logger = mkStdoutLogger
          _repository = mkStdoutRepository
          _controller = mkController
       in Env {_logger,  _repository, _controller}

Not very complicated, except... what is that weird `DepT Env IO` doing there in
the signature? 

Well, that's the whole reason this library exists. Trying to use a `ReaderT
(Env something) IO` to parameterize `Env` won't fly; you'll get weird "infinite
type" kind of errors because the `Env` needs to be parameterized with the monad
that provides the `Env` environment. So I created the `DepT` newtype over
`ReaderT` to mollify the compiler.

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

    usesLoggerD :: MonadDep '[HasLogger, HasRepository] d e m => Int -> m String
    usesLoggerD i = do
      e <- ask
      loggerD "I'm calling the logger!"
      return "foo"

Though perhaps it isn't worth the hassle.

## How to use "pure fakes" during testing?

The [test suite](./test/tests.hs) has an example of using a `Writer` monad for
collecting the outputs of functions working as ["test
doubles"](https://martinfowler.com/bliki/TestDouble.html).

## How to add "aspects" to functions in an environment?

The companion package
[dep-t-advice](http://hackage.haskell.org/package/dep-t-advice) provides a
general method extending the behaviour of `DepT`-effectful functions, in a
manner reminiscent of aspect-oriented programming.

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
  record. 

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

- [RIO](http://hackage.haskell.org/package/rio) is a featureful ReaderT-like /
  prelude replacement library which favors monomorphic environments.

- Another exploration of dependency injection with `ReaderT`:
  [ReaderT-OpenProduct-Environment](https://github.com/keksnicoh/ReaderT-OpenProduct-Environment).

- [registry](http://hackage.haskell.org/package/registry) is a package that
  implements an alternative approach to dependency injection, one different
  from the `ReaderT`-based one. 

