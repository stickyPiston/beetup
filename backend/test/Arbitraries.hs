module Arbitraries (genTimeStampsInOrder, genOccupanciesInOrder, genOccupanciesWithRange) where

import Data.Time (UTCTime(..), NominalDiffTime, diffUTCTime, addUTCTime)
import Data.Time.Clock (DiffTime)
import Data.Time.Calendar.OrdinalDate (Day, fromOrdinalDate)
import Test.Tasty.QuickCheck (Arbitrary, arbitrary, Gen, suchThat, scale, chooseAny, resize, elements, sized)
import HelperFunctions (halfHour, minutes)
import Utils.Datatypes (Occupancy(..))
import Data.Text (pack, Text)

{-- INSTANCES --}

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary Day where
  arbitrary = fromOrdinalDate <$> arbitrary <*> arbitrary

instance Arbitrary DiffTime where
  arbitrary = (fromIntegral . (*) 60) <$> (arbitrary :: Gen Int)

instance Arbitrary Occupancy where
  arbitrary = do
    title <- pack <$> arbitrary :: Gen Text
    ts <- genTimeStampsInOrder halfHour
    return $ Occupancy title (ts !! 0) (ts !! 1)

{-- GENRATORS --}

-- | Generates infinite random timestamps, in order, and not too close to each other
genTimeStampsInOrder :: NominalDiffTime -- ^ Minimum difference in time between timestamps
                     -> Gen [UTCTime]   -- ^ Generator of all those timestamps
genTimeStampsInOrder td = sized $ \s -> do
  zulu <- resize 0 arbitrary :: Gen UTCTime -- 0000-01-01
  let t = minutes s `addUTCTime` zulu
  ts <- suchThat (scale (*2) $ genTimeStampsInOrder td) $ \ts -> -- Enforce minimum time distance
    (head ts `diffUTCTime` t) > td
  return $ t : ts

-- | Generates infinite random occupancies, in order, and not too close to each other
genOccupanciesInOrder :: NominalDiffTime -- ^ Minimum difference in time between occupancies
                      -> Gen [Occupancy] -- ^ Generator of all those occupancies
genOccupanciesInOrder td = sized $ \size -> do
  title <- pack <$> elements ["Kaasbier", "Madelief Vazantje", "-02394  sdfpu\\32-94\nlaas"]
  ts <- genTimeStampsInOrder td
  let [start, end] = take 2 ts
  (:) (Occupancy title start end) <$> genOccupanciesInOrder td

-- | Generates a finite amount of occupancies, and gives a suitable start and end range to test the occupancies
genOccupanciesWithRange :: Gen ([Occupancy], UTCTime, UTCTime)
genOccupanciesWithRange = sized $ \s -> do
  occupancies   <- take (s + 3) <$> genOccupanciesInOrder halfHour
  let startRange = oStart $ head occupancies
  let endRange   = oEnd $ occupancies !! s
  return (occupancies, startRange, endRange)
