import Test.Tasty
import Test.Tasty.QuickCheck

import TimeSlotTests
import AvailabilityTests

main :: IO ()
main = defaultMain $ testGroup "All tests" $
    [timeSlotTests, availabilityTests]
