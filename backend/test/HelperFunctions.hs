module HelperFunctions (tally) where

-- import Test.Tasty.QuickCheck (Gen(..))
import Data.List (group, sort)

-- -- | Generates input data which scales slightly, to test the slow smoothPerms
-- -- implementation which scales badly
-- genPermutationArguments :: Gen (Int, [Int])
-- genPermutationArguments = do
--   max_distance <- arbitrary
--   xs_length <- sized (\x -> return $ 3 + (x `div` 15))
--   xs <- sequence $ replicate xs_length arbitrary
--   return (max_distance, xs)

-- | Determines frequencies of every item in a list
-- do something with... permutations?
tally :: (Ord a) => [a] -> [(a, Int)]
tally = map (\x -> (head x, length x)) . group . sort
