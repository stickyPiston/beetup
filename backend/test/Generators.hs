module Generators (genTimeStampsInOrder) where

import Arbitraries()

import Data.Time (UTCTime, NominalDiffTime, diffUTCTime)
import Test.Tasty.QuickCheck (Gen, arbitrary, suchThat, scale)

-- | Generates infinite random timestamps, in order, and not too close to each other
genTimeStampsInOrder :: NominalDiffTime -- ^ Minimum difference in time btween timestamps
                     -> Gen [UTCTime]   -- ^ Generator of all those timestamps
genTimeStampsInOrder td = scale (`div` 20) $ do
  t  <- arbitrary :: Gen UTCTime
  ts <- suchThat (genTimeStampsInOrder td) $ \ts ->
    (head ts `diffUTCTime` t) > td
  return $ t : ts
