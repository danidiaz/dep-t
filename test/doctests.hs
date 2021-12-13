module Main (main) where

import Test.DocTest
main = doctest [
    "-ilib", 
    "lib/Control/Monad/Dep.hs", 
    "lib/Control/Monad/Dep/Class.hs", 
    "lib/Dep/Has.hs", 
    "lib/Dep/Env.hs",
    "lib/Dep/Tagged.hs"
    ]

