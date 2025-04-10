module HelperFunctions (tally, steps, halfHour, minutes) where
import Data.Time (NominalDiffTime, secondsToNominalDiffTime)
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

minutes :: Pico -> NominalDiffTime
minutes n = secondsToNominalDiffTime (60 * n)

