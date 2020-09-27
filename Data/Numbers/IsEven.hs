module Data.Numbers.IsEven
    ( isEven, 
      isOdd 
    ) where

import Control.Monad.Trans.State.Lazy
import Data.Profunctor
import Data.Functor.Contravariant
import Data.List (foldl')
import Data.Bits

evenRec :: (Integral a) => a -> Bool
evenRec n = go n True
    where go 0 b = b
          go x b = go (pred x) (not b)
{-# INLINE evenRec #-}

evenFold :: (Integral a) => a -> Bool
evenFold = foldl' (const . not) False . enumFromTo 0
{-# INLINE evenFold #-}

_evenState :: Integral a => State a Bool
_evenState = do
    n <- get
    put (n - 1)
    return (toEnum ((fromIntegral n) `mod` 2))
{-# INLINE _evenState #-}

evenState :: (Integral a) => a -> Bool
evenState n = not . head $ evalState (sequence $ replicate (fromIntegral n) _evenState) n
{-# INLINE evenState #-}

evenIt :: (Integral a) => a -> Bool
evenIt = fst . (iterate (uncurry (flip (,))) (True, False) !!) . fromIntegral
{-# INLINE evenIt #-}

evenNaive :: (Integral a) => a -> Bool
evenNaive = even
-- TODO: refactor this complete mess of a function
{-# INLINE evenNaive #-}

data EvenP a b = EvenP (BoolC a) b  

newtype BoolC a = BoolC {getBool :: a -> Bool} 

instance Contravariant BoolC where
    contramap f (BoolC g) = BoolC (g . f)

instance Profunctor EvenP where
    dimap f g (EvenP a b) = EvenP (contramap f a) (g b)


evenProf :: (Integral a) => a -> Bool
evenProf n = let m = EvenP (BoolC not) 0
                 dmp = dimap not succ 
              in ((\(EvenP b _) -> getBool b) (head . dropWhile (\(EvenP _ z) -> z < n) . iterate dmp $ m)) $ False
-- build up a huge stack of nots from the contravariant argument of EvenP, finally applying it to False
{-# INLINE evenProf #-}

data Peano = S (Peano) | Z deriving (Eq, Show)

addPeano :: Peano -> Peano -> Peano
addPeano Z s = s
addPeano (S a) b = S (addPeano a b)

instance Ord Peano where
    Z     <= (S _) = True
    (S a) <= (S b) = a <= b

instance Enum Peano where
    toEnum x = (!! x) . iterate (addPeano (S Z)) $ Z
    fromEnum = length . takeWhile (/= Z) . iterate (\(S a) -> a)

evenPeano :: (Integral a) => a -> Bool
evenPeano = let go b Z = b
                go b (S a)  = go (not b) a
             in go True . toEnum . fromIntegral

evenBits :: (Integral a) => a -> Bool
evenBits = toEnum . xor 1 . (.&. 1) . fromIntegral

-- | Returns True if the number is even.
isEven :: (Integral a) => a -> Bool
isEven n = let a = abs n
               f = case (a `mod` 8) of
                     0 -> evenBits
                     1 -> evenPeano
                     2 -> evenProf
                     3 -> evenNaive
                     4 -> evenRec
                     5 -> evenFold
                     6 -> evenState
                     _ -> evenIt
            in f a
                 
-- | Returns True if the number is odd.
isOdd :: (Integral a) => a -> Bool
isOdd = not . isEven

