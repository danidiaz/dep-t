# dep-t

`DepT` is a
[ReaderT](http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html)-like
monad transformer for dependency injection.

The difference with `ReaderT` is that `DepT` takes an enviroment whose type is
parameterized by `DepT` itself.

## Rationale

To achieve dependency injection in Haskell, a common solution is to build a
record of functions and pass it to the program logic using a `ReaderT`.

Sometimes the functions in the record work directly in the `IO` monad.  But
sometimes we want to abstract over the monad. This can be achieved by
parameterizing the record by the monad:

    type Env :: (Type -> Type) -> Type
    data Env m = Env
      { logger :: String -> m (),
        logic :: Int -> m Int
      }

Let's write some implementations for the functions in the record. Notice that
some functions (like `_logic`) can depend on others (like `_logger`).

    _logger :: MonadIO m => String -> m ()
    _logger msg = liftIO (putStrLn msg)

    -- To avoid depending on the record type, we extract the logger with a getter.
    -- An axiliary HasLogger typeclass would also work.
    _logic :: MonadReader e m => (e -> String -> m ()) -> Int -> m Int
    _logic getLogger x = do
      logger <- reader getLogger
      logger "I'm going to multiply a number by itself!"
      return $ x * x

Now let's tie everything together using `ReaderT`:

    env' =
      Env
        { logger = _logger,
          logic = _logic logger
        }

    result' :: IO Int
    result' = runReaderT (logic env' 7) env'

Oops, this doesn't seem to work:

    * Couldn't match type `r0' with `Env (ReaderT r0 IO)'

What type should `env'` have? It seems to involve some weird infinite recursion
on types.

If we turn to `DepT`—which is just a newtype wrapper over `ReaderT` to pacify
the compiler—it works:

    env :: Env (DepT Env IO)
    env =
      Env
        { logger = _logger,
          logic = _logic logger
        }

    result :: IO Int
    result = runDepT (logic env 7) env

Notice that the use of `DepT` was limited to the moment of assembling the
record value and running one of its functions. The declaration of the record
type is independent of `DepT` (and of `ReaderT` for that matter). 

As for the implementation functions, they might use `MonadReader` to get hold
of the environment, but they know nothing of `DepT`, either.

## Links

- This library was extracted from my answer to [this Stack Overflow
  question](https://stackoverflow.com/a/61782258/1364288).

- The implementation of `mapDepT` was teased out in [this other SO question](https://stackoverflow.com/questions/65710657/writing-a-zooming-function-for-a-readert-like-monad-transformer).

- I'm unsure of the relationship between `DepT` and the technique described in
  [Adventures assembling records of
  capabilities](https://discourse.haskell.org/t/adventures-assembling-records-of-capabilities/623). 

  It seems that, with `DepT`, functions in the environment obtain their
  dependencies anew every time they are invoked. If we change a function in the
  environment record, all other functions which depend on it will be affected
  in subsequent invocations. I don't think this happens with "Adventures..." at
  least when changing an already "assembled" record.

