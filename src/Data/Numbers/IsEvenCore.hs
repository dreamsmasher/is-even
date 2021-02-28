{-# LANGUAGE FunctionalDependencies, FlexibleInstances, MultiParamTypeClasses, QuasiQuotes, TypeApplications #-}

module Data.Numbers.IsEvenCore
( evenRec
, evenFold
, evenState
, evenIt
, evenNaive
, evenLazy
, evenProf
, evenPeano
, toChurch
, evenChurch
, evenBits
, evenFixed
, evenGates
, evenRegex
, evenSemigroup
, evenMonoid
, evenGraph
) where

import Control.Monad.Trans.State.Strict ( execState, state )
import Control.Monad ( replicateM )
import Data.Function (fix)
import Data.Profunctor ( Profunctor(dimap) )
import Data.Semigroup (stimesMonoid)
import Data.Functor.Contravariant ( Contravariant(contramap) )
import Data.List (foldl')
import Data.Bits ( Bits((.&.), xor) )
import Text.RE.TDFA.String ( matched, re, (?=~) )

evenRec :: (Integral a) => a -> Bool
evenRec n = go n True
    where go 0 b = b
          go x b = go (pred x) (not b)
{-# INLINE evenRec #-}

evenFold :: (Integral a) => a -> Bool
evenFold = foldl' (const . not) False . enumFromTo 0
{-# INLINE evenFold #-}

evenState :: (Integral a) => a -> Bool
evenState n = execState (replicateM (fromIntegral n) flipper) True
    where flipper = state $ \b -> ((), not b)
{-# INLINE evenState #-}

evenIt :: (Integral a) => a -> Bool
evenIt = fst . (iterate (uncurry (flip (,))) (True, False) !!) . fromIntegral
{-# INLINE evenIt #-}

evenNaive :: (Integral a) => a -> Bool
evenNaive = even
-- TODO: refactor this complete mess of a function
{-# INLINE evenNaive #-}

-- essentially a graph coloring problem - 
-- figure out the parity of the list element at the nth position of [True, False, True, ..]
evenLazy :: (Integral a) => a -> Bool
evenLazy = last . zipWith const z . enumFromTo 0
    where z = True : False : z
{-# INLINE evenLazy #-}

data EvenP a b = EvenP 
    { contr :: BoolC a
    , covar :: b
    }

newtype BoolC a = BoolC { runBool :: a -> Bool }

instance Contravariant BoolC where
    contramap f = BoolC . (. f) . runBool

instance Profunctor EvenP where
    dimap f g (EvenP a b) = EvenP (contramap f a) (g b)


evenProf :: (Integral a) => a -> Bool
evenProf n = let m = EvenP (BoolC not) 0
                 dmp = dimap not succ 
              in (runBool . contr . head . dropWhile ((< n) . covar) . iterate dmp) m False
-- build up a huge stack of nots from the contravariant argument of EvenP, finally applying it to False
{-# INLINE evenProf #-}

data Peano = S Peano | Z deriving (Eq, Show)

addPeano :: Peano -> Peano -> Peano
addPeano Z s = s
addPeano (S a) b = S (addPeano a b)

instance Ord Peano where
    Z   <= _   = True
    S _ <= Z   = False
    S a <= S b = a <= b

instance Enum Peano where
    toEnum = (iterate (addPeano (S Z)) Z !!)
    fromEnum = length . takeWhile (/= Z) . iterate (\(S a) -> a)

evenPeano :: (Integral a) => a -> Bool
evenPeano = let go b Z = b
                go b (S a)  = go (not b) a
             in go True . toEnum . fromIntegral

newtype Church a b = Church { runChurch :: a -> b }

toChurch :: (a -> a) -> Peano -> Church a a
toChurch = (Church .) . go
    where go _ Z = id
          go h (S a) = h . go h a

evenChurch :: (Integral a) => a -> Bool
evenChurch = flip (runChurch . toChurch not . toEnum . fromIntegral ) True
-- toChurch is just a more generalized version of evenPeano where any function can be applied
{-# INLINE evenChurch #-}

evenBits :: (Integral a) => a -> Bool
evenBits = toEnum . xor 1 . (.&. 1) . fromIntegral
{-# INLINE evenBits #-}

(?) :: Bool -> a -> a -> a
(?) True = const
(?) False = const id

-- thank you Haskell Curry
evenFixed :: (Integral a) => a -> Bool
evenFixed = (fix $ \f b x -> (x == 0) ? b $ f (not b) (x - 1)) True -- (fix $ \f b x -> if x == 0 then b else f (not b) (x - 1)) True
-- {-# INLINE evenFixed #-}

class Nand a b c | a b -> c where
    (|@.) :: a -> b -> c

instance Nand Bool Bool Bool where
    True |@. True = False
    _    |@. _    = True

infixr 8 |@.

class Xor a b c | a b -> c where
    (||>) :: a -> b -> c

instance Xor Bool Bool Bool where
    a ||> b = (a |@. ab) |@. (b |@. ab)
        where ab = a |@. b
              

evenGates :: (Integral a) => a -> Bool
evenGates = foldr (||>) True . flip replicate True . fromIntegral
{-# INLINE evenGates #-}

evenRegex :: (Integral a) => a -> Bool
evenRegex = matched . (?=~ [re|\d*[02468]$|]) . show @Integer . fromIntegral 
{-# INLINE evenRegex #-}

instance Semigroup (BoolC Bool) where
    n1 <> n2 = BoolC (runBool n1 . runBool n2)


instance Monoid (BoolC Bool) where
    mempty = BoolC id

evenMonoid :: (Integral a) => a -> Bool
evenMonoid n = runBool (mconcat $ replicate (fromIntegral n) (BoolC not)) True
{-# INLINE evenMonoid #-}

data Parity = Even | Odd deriving (Eq, Show, Enum, Ord)

instance Semigroup Parity where
    Odd <> Odd = Even
    Even <> Even = Even
    _ <> _ = Odd

instance Monoid Parity where
    mempty = Even

-- equivalent to evenBits
-- this is actually a lie since we're using monoids here too
evenSemigroup :: (Integral a) => a -> Bool
evenSemigroup = (== Even) . (`stimesMonoid` Odd)
{-# INLINE evenSemigroup #-}

-- create a cyclic list containing n to the nearest even number elements, then traverse 
-- if we end up where we started, the number is even
-- similar to evenLazy - i'm running out of ideas
evenGraph :: (Integral a) => a -> Bool
evenGraph 0 = True
evenGraph 1 = False
evenGraph n = (tail ring !! fromIntegral n) == 0
    where ring = [0..n `div` 2 * 2] <> ring
{-# INLINE evenGraph #-}

-- TODO: implementation based on goldbach's conjecture
-- all even numbers > 2 = (p1 + p2), where p1 and p2 are some primes
