import Test.Tasty

import TimeSlotTests
import AvailabilityTests

main :: IO ()
main = defaultMain $ testGroup "All tests" $
    [timeSlotTests, availabilityTests]

