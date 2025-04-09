module AvailabilityTests (test) where

import HelperFunctions

import Test.Tasty
import Test.Tasty.QuickCheck

test :: TestTree
test = testProperty "Test" $ const True

-- smoothPermsProperties :: TestTree
-- smoothPermsProperties = testGroup "Tree implementation properties" $
--   [isSmooth, preservedContents, preservedLength, samePermsAsSlow, sameSmoothPermsAsSlow]

-- isSmooth :: TestTree
-- isSmooth = testProperty "Permutations are smooth" $ forAll genPermutationArguments $ \(d, xs) ->
--   let permutations = smoothPerms d xs :: [[Int]]
--   in not (null permutations) ==> all (smooth d) permutations

-- preservedContents :: TestTree
-- preservedContents = testProperty "Permutations preserve original contents" $ forAll genPermutationArguments $ \(d, xs) ->
--   let targetTally = tally xs :: [(Int, Int)]
--   in all ((==) targetTally . tally) (smoothPerms d xs)

-- preservedLength :: TestTree
-- preservedLength = testProperty "Permutations have proper length" $ forAll genPermutationArguments $ \(d, xs) ->
--   let permutations = smoothPerms d xs
--       targetLength = length xs
--    in not (null permutations) ==> all ((==) targetLength . length) permutations

-- samePermsAsSlow :: TestTree
-- samePermsAsSlow = testProperty "Tree permutations are equal to naive permutations" $ forAll genPermutationArguments $ \(_, xs) ->
--    Slow.perms xs == permTreeToPerms (listToPermTree xs)

-- sameSmoothPermsAsSlow :: TestTree
-- sameSmoothPermsAsSlow = testProperty "Tree smooth permutations are equal to naive smooth permutations" $ forAll genPermutationArguments $ \(d, xs) ->
--    Slow.smoothPerms d xs == smoothPerms d xs
