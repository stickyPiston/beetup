import Test.Tasty
import Test.Tasty.QuickCheck

import TimeSlotTests

main :: IO ()
main = defaultMain $ testGroup "All tests" $
    [timeSlotTests]
