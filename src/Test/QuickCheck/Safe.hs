-- | This module implements a simplified, pure version of Test.Quickcheck's
-- quickCheck functionality.

-- Author: Bertram Felgenhauer
-- License: MIT

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

module Test.QuickCheck.Safe (
    -- * Checking properties
    quickCheck, quickCheckResult, quickCheckWith, quickCheckWithResult,
    -- * Creating and combining properties
    STestable(),
    (==>), (.||.), (.&&.), (.&.), (===),
    label, shrinking, noShrinking, mapSize,
    forAll, forAllShrink,
    -- * Miscellaneous
    inventQCGen,
    module Test.QuickCheck
) where

import Test.QuickCheck.Safe.Trusted

import Test.QuickCheck hiding (
    Testable(..), Property(..),
    (==>), (.||.), (.&&.), (.&.), (===),
    label, shrinking, noShrinking, mapSize,
    forAll, forAllShrink,
    classify, collect, conjoin, counterexample, cover, disjoin,
    expectFailure, once, printTestCase, verbose, within,
    quickCheck, quickCheckResult, quickCheckWith, quickCheckWithResult)
import Test.QuickCheck.Gen (Gen(..))
import Control.Monad

-- STestable and SProperty are simplified versions of Testable/Property
class STestable prop where
    sProperty :: prop -> SProperty

newtype SProperty = MkSProperty{ unSProperty :: Gen SResult }

data SResult
    = SOk                                -- success
    | SDiscard                           -- discarded sample
    | SFail{                             -- failed sample
        sLabels :: [String],             -- text describing counterexample
        sException :: Maybe AnException, -- caught exception, if any
        sSmaller :: [SResult]            -- results of shrunk examples
    }

instance STestable SProperty where
    sProperty prop = prop

instance STestable prop => STestable (Gen prop) where
    sProperty gen = MkSProperty $ gen >>= unSProperty . sProperty

-- instance STestable Discard where
--     sProperty _ = MkSProperty . return $ SDiscard

instance STestable Bool where
    sProperty b = MkSProperty . return $ case pureEvaluate b of
        Right True -> SOk
        Right _ -> SFail{ sLabels = [], sException = Nothing, sSmaller = [] }
        Left e -> SFail{ sLabels = [], sException = Just e, sSmaller = [] }

instance (Arbitrary a, Show a, STestable prop) => STestable (a -> prop) where
    sProperty = forAllShrink arbitrary shrink

-- | Implication. Cf. 'Test.QuickCheck.==>'.
(==>) :: STestable prop => Bool -> prop -> SProperty
t ==> p = case pureEvaluate t of
    Right True -> sProperty $ p
    Right _ -> MkSProperty . return $ SDiscard
    Left e -> MkSProperty . return $
        SFail{ sLabels = [], sException = Just e, sSmaller = [] }

-- | Equality test. Cf. 'Test.QuickCheck.==='.
(===) :: (Eq a, Show a) => a -> a -> SProperty
a === b = label (show a ++ " /= " ++ show b) $ sProperty (a == b)

-- | Conjunction. Cf. 'Test.QuickCheck..&&.'.
(.&&.) :: (STestable prop2, STestable prop1) => prop1 -> prop2 -> SProperty
prop1 .&&. prop2 = MkSProperty $ do
    res1 <- unSProperty $ label "LHS" $ prop1
    case res1 of
        SOk -> unSProperty $ label "RHS" $ prop2
        _ -> return res1

-- | Disjunction. Cf. 'Test.QuickCheck..||.'.
(.||.) :: (STestable prop2, STestable prop1) => prop1 -> prop2 -> SProperty
prop1 .||. prop2 = MkSProperty $ do
    res1 <- unSProperty . sProperty $ prop1
    res2 <- unSProperty . sProperty $ prop2
    let merge res1@SFail{ sSmaller = shr1 } res2@SFail{ sSmaller = shr2 } =
            SFail{
                sLabels = sLabels res1 ++ sLabels res2,
                sException = sException res1 `mplus` sException res2,
                sSmaller = map (`merge` res2) shr1 ++ map (res1 `merge`) shr2
            }
        merge res1 SFail{} = res1
        merge SFail{} res2 = res2
    return $ res1 `merge` res2

-- | Nondeterministic conjunction. Cf. 'Test.QuickCheck.&.'.
(.&.) :: (STestable prop2, STestable prop1) => prop1 -> prop2 -> SProperty
prop1 .&. prop2 = MkSProperty $ do
    c <- choose (0, 1)
    case c :: Int of
        0 -> unSProperty $ label "LHS" prop1
        1 -> unSProperty $ label "RHS" prop2

-- | Label tests. Cf. 'Test.QuickCheck.label'.
label :: STestable prop => String -> prop -> SProperty
label lab = MkSProperty . fmap (labelSResult lab) . unSProperty . sProperty

labelSResult :: String -> SResult -> SResult
labelSResult lab = mapSResultLabels (lab :)

mapSResultLabels :: ([String] -> [String]) -> SResult -> SResult
mapSResultLabels f res@SFail{} = res{
    sLabels = f (sLabels res),
    sSmaller = map (mapSResultLabels f) (sSmaller res)
 }
mapSResultLabels _ res = res

-- | Shrink counterexamples. Cf. 'Test.QuickCheck.shrinking'.
shrinking :: STestable prop => (a -> [a]) -> a -> (a -> prop) -> SProperty
shrinking shr x f = MkSProperty $ MkGen $ \seed size -> do
    let unfold x = case unGen (unSProperty . sProperty $ f x) seed size of
            res@SFail{ sSmaller = ps } ->
                res{ sSmaller = map unfold (shr x) ++ sSmaller res }
            res -> res
    unfold x

-- | Suppress shrinking of counterexamples. Cf. 'Test.QuickCheck.noShrinking'.
noShrinking :: STestable prop => prop -> SProperty
noShrinking prop = MkSProperty $ do
    res <- unSProperty . sProperty $ prop
    return $ case res of
        SFail{} -> res{ sSmaller = [] }
        _ -> res

-- | Universal quantification with shrinking.
-- Cf. 'Test.QuickCheck.forAllShrink'.
forAllShrink :: (Show a, STestable prop) =>
    Gen a -> (a -> [a]) -> (a -> prop) -> SProperty
forAllShrink gen shr f = MkSProperty $ do
    x <- gen
    unSProperty . label (show x) $ shrinking shr x f

-- | Universal quantification. Cf. 'Test.QuickCheck.forAll'.
forAll :: (Show a, STestable prop) => Gen a -> (a -> prop) -> SProperty
forAll gen = forAllShrink gen (const [])

-- | Adjust testcase sizes. Cf. 'Test.QuickCheck.mapSize'.
mapSize :: STestable prop => (Int -> Int) -> prop -> SProperty
mapSize f = MkSProperty . scale f . unSProperty . sProperty where
    scale f a = sized (\n -> resize (f n) a)

-- Other combinators that may be considered:

-- classify :: STestable prop => Bool -> String -> prop -> SProperty
-- collect :: (Show a, STestable prop) => a -> prop -> SProperty
-- conjoin :: STestable prop => [prop] -> SProperty
-- counterexample :: STestable prop => String -> prop -> SProperty
-- cover :: STestable prop => Bool -> Int -> String -> prop -> SProperty
-- disjoin :: STestable prop => [prop] -> SProperty
-- expectFailure :: STestable prop => prop -> SProperty
-- once :: STestable prop => prop -> SProperty
-- printTestCase :: STestable prop => String -> prop -> SProperty
-- verbose :: STestable prop => prop -> SProperty
-- within :: STestable prop => Int -> prop -> SProperty

-- | Cf. 'Test.QuickCheck.quickCheckWithResult'. Note that in contrast to
-- QuickCheck's function, this one takes an additional 'QCGen' argument.
quickCheckWithResult :: STestable prop => Args -> QCGen -> prop -> Result
quickCheckWithResult args seed prop = unGen (runTests 0 0 sizes) seed' 0 where
    runTests :: Int -> Int -> [Int] -> Gen Result
    runTests pass disc (size : sizes)
        | pass >= maxSuccess args =
            return Success{
                numTests = pass,
                labels = [],
                output = "+++ OK, passed " ++ show pass ++ " tests.\n"
             }
        | disc > (maxDiscardRatio args - 1) * maxSuccess args =
            return GaveUp{
                numTests = pass,
                labels = [],
                output = "*** Gave up! Passed only " ++ show pass ++ " tests.\n"
             }
        | otherwise = do
            (seed, _) <- MkGen (,)
            res <- resize size (unSProperty . sProperty $ prop)
            case res of
                SOk -> runTests (pass + 1) disc sizes
                SDiscard -> runTests pass (disc + 1) sizes
                SFail{} -> return $ deflate pass 0 0 0 seed size res

    deflate :: Int -> Int -> Int -> Int -> QCGen -> Int -> SResult -> Result
    deflate pass !shr !shrT !shrF seed size res@SFail{ sSmaller = [] } =
        Failure{
            numTests = pass,
            numShrinks = shr,
            numShrinkTries = shrT,
            numShrinkFinal = shrF,
            usedSeed = seed,
            usedSize = size,
            reason = reason,
            theException = sException res,
            labels = map (\x -> (x, 0)) (sLabels res),
#if MIN_VERSION_QuickCheck(2,10,0)
            failingTestCase = sLabels res,
#endif
            output = "*** Failed! " ++ reason ++
                  " (after " ++ count (pass + 1) "test" ++
                  (if shr > 0 then " and " ++ count shr "shrink" else "") ++
                  "):\n" ++ unlines (sLabels res)
        }
      where
        count i w = show i ++ " " ++ w ++ ['s' | i /= 1]
        reason = maybe "Falsifiable" (\e -> "Exception: '" ++ show e ++ "'") $
            sException res
    deflate pass shr shrT shrF seed size res@SFail{ sSmaller = res' : rs } =
        case res' of
            SFail{} -> deflate pass (shr + 1) (shrT + shrF) 0 seed size res'
            _ -> deflate pass shr shrT (shrF + 1) seed size res{ sSmaller = rs }

    sizes :: [Int]
    sizes = cycle [0..maxSize args]

    seed' :: QCGen
    seed' = maybe seed fst (replay args)

-- | Cf. 'Test.QuickCheck.quickCheckResult'. Note that in contrast to
-- QuickCheck's function, this one takes an additional 'QCGen' argument.
quickCheckResult :: STestable prop => QCGen -> prop -> Result
quickCheckResult = quickCheckWithResult stdArgs

-- | Cf. 'Test.QuickCheck.quickCheckWith'. Note that in contrast to
-- QuickCheck's function, this one takes an additional 'QCGen' argument.
quickCheckWith :: STestable prop => Args -> QCGen -> prop -> String
quickCheckWith args seed = output . quickCheckWithResult args seed

-- | Cf. 'Test.QuickCheck.quickCheck'. Note that in contrast to QuickCheck's
-- function, this one takes an additional 'QCGen' argument.
--
-- >>> putStr $ quickCheck (inventQCGen ()) (\x -> length (x :: [()]) < 10)
-- *** Failed! Falsifiable (after 18 tests and 3 shrinks):
-- [(),(),(),(),(),(),(),(),(),(),(),(),(),(),()]
quickCheck :: STestable prop => QCGen -> prop -> String
quickCheck = quickCheckWith stdArgs

