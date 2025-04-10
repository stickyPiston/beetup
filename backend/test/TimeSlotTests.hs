module TimeSlotTests (timeSlotTests) where

import HelperFunctions (halfHour, steps)
import Availability (timeSlots)
import Utils.Datatypes (earlier, overlaps, valid)
import Arbitraries()
import Generators

import Test.Tasty
import Test.Tasty.QuickCheck

timeSlotTests :: TestTree
timeSlotTests = testGroup "Tests generation of timeslots between given timestamps" $
    [timeSlotsValid, timeSlotsDoNotOverlap, timeSlotsAreOrdered]

timeSlotsValid :: TestTree
timeSlotsValid = testProperty "TimeSlots are valid" $ forAll (genTimeStampsInOrder halfHour) $ \ts ->
    let slots = timeSlots halfHour (ts !! 0) (ts !! 1)
    in all valid slots

timeSlotsDoNotOverlap :: TestTree
timeSlotsDoNotOverlap = testProperty "TimeSlots do not overlap" $ forAll (genTimeStampsInOrder halfHour) $ \ts ->
    let slots = timeSlots halfHour (ts !! 0) (ts !! 1)
    in all (not . uncurry overlaps) $ steps slots

timeSlotsAreOrdered :: TestTree
timeSlotsAreOrdered = testProperty "TimeSlots are in order" $ forAll (genTimeStampsInOrder halfHour) $ \ts ->
    let slots = timeSlots halfHour (ts !! 0) (ts !! 1)
    in all (uncurry earlier) $ steps slots


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
