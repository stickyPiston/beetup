module TimeSlotTests (timeSlotTests) where

import HelperFunctions (halfHour, steps)
import Availability (timeSlots)
import Utils.Datatypes (earlier, overlaps, valid)
import Arbitraries

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

