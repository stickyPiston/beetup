module HelperFunctions (tally, steps, halfHour, minutes) where
import Data.Time (NominalDiffTime)
import Data.Fixed (Pico)

-- import Test.Tasty.QuickCheck (Gen(..))
import Data.List (group, sort)

-- | Determines frequencies of every item in a list
tally :: (Ord a) => [a] -> [(a, Int)]
tally = map (\x -> (head x, length x)) . group . sort

steps :: [a] -> [(a, a)]
steps xs = zip xs (tail xs)

halfHour :: NominalDiffTime
halfHour = secondsToNominalDiffTime (60 * 30)

minutes :: Int -> NominalDiffTime
minutes n = fromIntegral (60 * n)

