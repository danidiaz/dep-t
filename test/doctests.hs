module Main (main) where

import Test.DocTest
main = doctest [
    "-ilib", 
    "lib/Control/Monad/Dep.hs", 
    "lib/Control/Monad/Dep/Class.hs", 
    "lib/Dep/Has.hs", 
    "lib/Dep/Injects.hs", 
    "lib/Dep/Env.hs",
    "lib/Dep/Phases.hs", 
    "lib/Dep/Constructor.hs",
    "lib/Dep/Tagged.hs"
    ]

