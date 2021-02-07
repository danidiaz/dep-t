module Main (main) where

import Test.DocTest
main = doctest ["-ilib", "lib/Control/Monad/Dep.hs", "lib/Control/Monad/Dep/Class.hs", "lib/Control/Monad/Dep/Has.hs"]

