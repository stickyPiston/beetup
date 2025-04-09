{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators #-}

module Integration.Init where

import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import Data.Time (UTCTime)
import Data.Text (Text)
import Database.Persist.Sqlite (runMigration, runSqlPersistMPool)
import Utils.Endpoint (DBPool)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

UserEntity
  name Text
  username Text
  password Text
  deriving (Show)

OccupancyEntity
  title Text
  start UTCTime
  end UTCTime
  userId UserEntityId
  deriving (Show)

AvailabilityEntity
  start UTCTime
  end UTCTime
  userId UserEntityId
  deriving (Show)

MeetingEntity
  meetingId Text
  title Text
  start UTCTime  -- Only the time component is used
  end UTCTime    -- Only the time component is used
  days [UTCTime] -- Only the date component is used
  userId UserEntityId
  availabilities [AvailabilityEntity]
  description Text
  deriving (Show)
|]

initDB :: DBPool -> IO ()
initDB = runSqlPersistMPool (runMigration migrateAll)
