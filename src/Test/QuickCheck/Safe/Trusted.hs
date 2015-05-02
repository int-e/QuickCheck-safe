-- Author: Bertram Felgenhauer
-- License: MIT

{-# LANGUAGE Trustworthy #-}

module Test.QuickCheck.Safe.Trusted (
    pureEvaluate, AnException,
    inventQCGen, QCGen
) where

import Test.QuickCheck.Exception
import Test.QuickCheck.Random
import System.IO.Unsafe

-- | 'pureEvaluate' wraps 'Test.QuickCheck.Exception.tryEvaluate' in
-- 'System.IO.Unsafe.unsafePerformIO'. This may look like a dirty hack,
-- but this building block allows us to implement most of QuickCheck's
-- functionality without resorting to IO again.
pureEvaluate :: a -> Either AnException a
pureEvaluate = unsafePerformIO . tryEvaluate

-- | 'inventQCGen' invokes 'Test.QuickCheck.Random.newQCGen' via
-- 'unsafePerformIO'. It is useful in connection with the
-- 'Test.QuickCheck.Safe.quickCheck' family of functions.
{-# NOINLINE inventQCGen #-}
inventQCGen :: a -> QCGen
inventQCGen _ = unsafePerformIO newQCGen
