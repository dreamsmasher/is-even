import Test.QuickCheck
import Data.Numbers.IsEven

propEven :: [Int] -> Bool
propEven xs = map isEven xs == map even xs


propOdd :: [Int] -> Bool
propOdd xs = map isOdd xs == map odd xs


main :: IO ()
main = do
    verboseCheck propEven
    verboseCheck propOdd
