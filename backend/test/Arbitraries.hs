module Arbitraries (genTimeStampsInOrder, genOccupanciesInOrder, genOccupanciesWithRange) where

import Data.Time (UTCTime(..), NominalDiffTime, diffUTCTime)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.Calendar.OrdinalDate (Day, fromOrdinalDate)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, Gen, suchThat, scale, chooseAny)
import HelperFunctions (halfHour)
import Utils.Datatypes (Occupancy(..))
import Data.Text (pack, Text)

{-- INSTANCES --}

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary Day where
  arbitrary = fromOrdinalDate <$> arbitrary <*> arbitrary

instance Arbitrary DiffTime where
  arbitrary = (secondsToDiffTime . (*) 60) <$> arbitrary

instance Arbitrary Occupancy where
  arbitrary = do
    title <- pack <$> arbitrary :: Gen Text
    ts <- genTimeStampsInOrder halfHour
    return $ Occupancy title (ts !! 0) (ts !! 1)

{-- GENRATORS --}

-- | Generates infinite random timestamps, in order, and not too close to each other
genTimeStampsInOrder :: NominalDiffTime -- ^ Minimum difference in time between timestamps
                     -> Gen [UTCTime]   -- ^ Generator of all those timestamps
genTimeStampsInOrder td = scale (`div` 20) $ do
  t  <- arbitrary :: Gen UTCTime
  ts <- suchThat (genTimeStampsInOrder td) $ \ts ->
    (head ts `diffUTCTime` t) > td
  return $ t : ts

-- | Generates infinite random occupancies, in order, and not too close to each other
genOccupanciesInOrder :: NominalDiffTime -- ^ Minimum difference in time between occupancies
                      -> Gen [Occupancy] -- ^ Generator of all those occupancies
genOccupanciesInOrder td = (:) <$> arbitrary <*> genOccupanciesInOrder td

-- | Generates a finite amount of occupancies, and gives a suitable start and end range to test the occupancies
genOccupanciesWithRange :: Gen ([Occupancy], UTCTime, UTCTime)
genOccupanciesWithRange = do
    numberOfOccupancies <- (+3) <$> suchThat arbitrary (>0)
    occupancies   <- take numberOfOccupancies <$> genOccupanciesInOrder halfHour
    let startRange = oStart $ head occupancies
    let endRange   = oEnd $ occupancies !! (numberOfOccupancies - 2)
    return (occupancies, startRange, endRange)
