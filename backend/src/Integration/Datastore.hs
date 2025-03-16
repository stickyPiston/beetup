
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

module Integration.Datastore where

import Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import Data.Time (Day, TimeOfDay)
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigration, toSqlKey)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Text
  username Text
  password Text
  deriving (Show)

Occupancy
  title Text
  date Day
  startTime TimeOfDay
  endTime TimeOfDay
  userId UserId
  deriving (Show)
|]

initDB :: IO ()
initDB = runSqlite "main.db" $ do
  runMigration migrateAll

  -- Test Data
  -- jortId <- insert $ User "Jort" "jortw" "helloworld"

  return ()

findUserByUsername :: Text -> IO (Maybe (Entity User))
findUserByUsername uname = runSqlite "main.db" $ selectFirst [UserUsername ==. uname] []

findUserById :: Int -> IO (Maybe (Entity User))
findUserById id = runSqlite "main.db" $ selectFirst [UserId ==. toSqlKey (fromIntegral id)] []

insertUser :: User -> IO UserId
insertUser u = runSqlite "main.db" $ insert u
