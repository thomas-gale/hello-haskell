import Data.Char
import LearnLib
import Test.QuickCheck

prop_isAllDigits :: [Char] -> Bool
prop_isAllDigits val =
  if isAllDigits val || val == ""
    then onlyDigit == length val
    else onlyDigit /= length val
  where
    onlyDigit = length $ filter isDigit val

main :: IO ()
main = do
  quickCheck prop_isAllDigits
  putStrLn "Tests complete"
