module Data.Numbers.IsEven 
    ( isEven
    , isOdd
    ) where

import System.Random ( mkStdGen, Random(randomRs) )
import Data.Numbers.IsEvenCore
    ( evenBits
    , evenPeano
    , evenProf
    , evenNaive
    , evenRec
    , evenFold
    , evenState
    , evenChurch
    , evenLazy
    , evenGates
    , evenFixed
    , evenRegex
    , evenIt
    , evenSemigroup
    , evenMonoid
    , evenGraph
    )

-- | Returns True if the number is even.
isEven :: (Integral a) => a -> Bool
isEven n = 
    let a = (abs . fromIntegral) n
        g = mkStdGen a
        f = case ((!! a) . randomRs (0 :: Int, 15)) g of
            0  -> evenBits
            1  -> evenPeano
            2  -> evenProf
            3  -> evenNaive
            4  -> evenRec
            5  -> evenFold
            6  -> evenState
            7  -> evenChurch
            8  -> evenLazy
            9  -> evenGates
            10 -> evenFixed
            11 -> evenRegex
            12 -> evenSemigroup
            13 -> evenMonoid
            14 -> evenGraph
            _  -> evenIt
    in f a
{-# INLINE isEven #-}

-- | Returns True if the number is odd.
isOdd :: (Integral a) => a -> Bool
isOdd = not . isEven
{-# INLINE isOdd #-}
