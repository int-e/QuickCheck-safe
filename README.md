QuickCheck-safe reimplements the quickCheck functionality with a pure
interface and a very small trusted core (see Test.QuickCheck.Safe.Trusted).

Note that the Test.QuickCheck.Arbitrary functionality is reused.

The package is targeted at users who want to leverage SafeHaskell for
sandboxing.

```
> putStr $ quickCheck (inventQCGen ()) (\x -> length (x :: [()]) < 10)
*** Failed! Falsifiable (after 18 tests and 3 shrinks):
[(),(),(),(),(),(),(),(),(),(),(),(),(),(),()]
```
