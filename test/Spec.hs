import Test.QuickCheck
import Test.Hspec
import Test.Hspec.Runner
import Test.Hspec.QuickCheck
import Data.Numbers.IsEven

propEven :: [Int] -> Bool
propEven xs = map isEven xs == map even xs


propOdd :: [Int] -> Bool
propOdd xs = map isOdd xs == map odd xs


spec :: Spec
spec = do
    modifyMaxSuccess (const 1000) $ it "checks if a number is even" $ property propEven
    modifyMaxSuccess (const 1000) $ it "checks if a number is odd" $ property propOdd

config :: Config
config = defaultConfig {
        configPrintCpuTime = True,
        configRandomize = True,
        configColorMode = ColorAuto

                       }
main :: IO ()
main = readConfig config [] >>= runSpec spec >>= evaluateSummary

