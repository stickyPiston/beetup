import Test.Tasty
import Test.Tasty.QuickCheck
import qualified AvailabilityTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Availability tests" [AvailabilityTests.test1]
