{-# LANGUAGE TypeApplications #-}
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.QuickCheck
import Data.Numbers.IsEven
import Data.Word (Word)

propEven :: Integral a => a -> Bool
propEven x = isEven x == even x

propOdd :: Integral a => a -> Bool
propOdd x = isOdd x == odd x

specEven :: Spec
specEven = describe "isEven" $ do
    it "works for Ints" $ property (propEven @Int)
    it "works for Integers" $ property (propEven @Integer)
    it "works for Words" $ property (propEven @Word)

specOdd :: Spec
specOdd = describe "isOdd" $ do
    it "works for Ints" $ property (propOdd @Int)
    it "works for Integers" $ property (propOdd @Integer)
    it "works for Words" $ property (propOdd @Word)

spec :: Spec
spec = mapM_ (modifyMaxSuccess $ const 5000) [specEven, specOdd]

config :: Config
config = defaultConfig 
    { configPrintCpuTime = True
    , configRandomize = True
    , configColorMode = ColorAuto
    }

main :: IO ()
main = readConfig config [] >>= runSpec spec >>= evaluateSummary

