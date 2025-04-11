module AvailabilityTests (availabilityTests) where

import HelperFunctions (halfHour, steps)
import Availability (timeSlots, determineAvailabilites)
import Utils.Datatypes (earlier, overlaps, valid)
import Arbitraries
import Data.Maybe (maybe)

import Test.Tasty
import Test.Tasty.QuickCheck

availabilityTests :: TestTree
availabilityTests = testGroup "Tests generation of availabilities based on occupancies" $
                      [availabilitiesValid]

availabilitiesValid :: TestTree
availabilitiesValid = testProperty "Availabilities are valid" $
  forAll genOccupanciesWithRange $ \(occupancies, start, end) ->
    let availabilities = determineAvailabilites start end occupancies 21
    in maybe False (all valid) availabilities

availabilitiesDoNotOverlap :: TestTree
availabilitiesDoNotOverlap = testProperty "Availabilities are valid" $
  forAll genOccupanciesWithRange $ \(occupancies, start, end) ->
    let availabilities = determineAvailabilites start end occupancies 21
    in maybe False (all (not . uncurry overlaps) . steps) availabilities
