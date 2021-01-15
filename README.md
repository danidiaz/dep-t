
    $ ghc --version
    The Glorious Glasgow Haskell Compilation System, version 8.10.3

With `O0` compiles ok:

    $ ghc -O0 Main.hs
    Linking Main.exe ...

With `O1` fails:

    $ ghc -O1 Main.hs
    [1 of 1] Compiling Main             ( Main.hs, Main.o ) [Optimisation flags changed]
    Simplifier ticks exhausted
      When trying UnfoldingDone ds_s1ze
      To increase the limit, use -fsimpl-tick-factor=N (default 100).

      If you need to increase the limit substantially, please file a
      bug report and indicate the factor you needed.

      If GHC was unable to complete compilation even with a very large factor
      (a thousand or more), please consult the "Known bugs or infelicities"
      section in the Users Guide before filing a report. There are a
      few situations unlikely to occur in practical programs for which
      simplifier non-termination has been judged acceptable.

      To see detailed counts use -ddump-simpl-stats
      Total ticks: 35843


