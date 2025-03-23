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
import Data.Time (Day, TimeOfDay)
import Data.Text (Text)
import Database.Persist.Sqlite (runSqlite, runMigration)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserEntity
  name Text
  username Text
  password Text
  deriving (Show)

OccupancyEntity
  title Text
  date Day
  startTime TimeOfDay
  endTime TimeOfDay
  userId UserEntityId
  deriving (Show)
|]

initDB :: IO ()
initDB = runSqlite "main.db" $ do
  runMigration migrateAll

  -- Test Data
  -- jortId <- insert $ UserEntity "Jort" "jortw" "helloworld"

  return ()
