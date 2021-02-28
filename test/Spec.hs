{-# LANGUAGE ExistentialQuantification, TypeApplications #-}
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.QuickCheck
import System.Random

import Data.Numbers.IsEven
import Data.Numbers.IsEvenCore

propEven :: forall a. (Integral a, Arbitrary a) => (a -> Bool) -> a -> Bool
propEven f x = f x == even x

propOdd :: Integral a => a -> Bool
propOdd x = isOdd x == odd x

specEvenFuncs ::  String -> (Int -> Bool) -> Spec
specEvenFuncs s f = parallel $ describe s $ do
    it "checks if a number is even" 
      $ property (forAll (chooseInt (0, 1000000)) $ propEven f)

specIsEven, specOdd :: Spec
specIsEven = parallel $ describe "isEven" $ do
    it "checks if a number is even" $ property (propEven (isEven @Integer))

specOdd = parallel $ describe "isOdd" $ do
    it "checks if a number is odd" $ property (propOdd @Integer)

funcs :: (Integral a, Arbitrary a, Show a) => [a -> Bool]
funcs = [ evenBits
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
        , evenMonoid
        , evenSemigroup
        , evenGraph
        ]

names :: [String]
names = [ "evenBits"
        , "evenPeano"
        , "evenProf"
        , "evenNaive"
        , "evenRec"
        , "evenFold"
        , "evenState"
        , "evenChurch"
        , "evenLazy"
        , "evenGates"
        , "evenFixed"
        , "evenRegex"
        , "evenIt"
        , "evenMonoid"
        , "evenSemigroup"
        , "evenGraph"
        ]

spec :: Spec
spec = parallel $ do
    mapM_ (modifyMaxSuccess $ const 1000) $ 
        zipWith specEvenFuncs names funcs <> [specIsEven, specOdd]

config :: Config
config = defaultConfig 
    { configPrintCpuTime = True
    , configRandomize = True
    , configColorMode = ColorAuto
    }

main :: IO ()
main = readConfig config [] >>= runSpec spec >>= evaluateSummary

