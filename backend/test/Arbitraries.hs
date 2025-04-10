module Arbitraries () where

import Data.Time (UTCTime(..))
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.Calendar.OrdinalDate (Day, fromOrdinalDate)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary)

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary Day where
    arbitrary = fromOrdinalDate <$> arbitrary <*> arbitrary

instance Arbitrary DiffTime where
    arbitrary = (secondsToDiffTime . (*) 60) <$> arbitrary
