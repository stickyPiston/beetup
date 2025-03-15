
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
import Database.Persist.Sqlite (runSqlite, runMigration, PersistStoreWrite (insert))
import Data.Time (Day, TimeOfDay)
import Data.Text (Text)

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

  jortId <- insert $ User "Jort" "jortw" "helloworld"

  return ()

-- selectUser :: Connection  -- database connection
--            -> String      -- username
--            -> Maybe User  -- return user if found
-- selectUser = undefined

-- selectAllUsers :: Connection -> Maybe [User]
-- selectAllUsers = undefined

-- insertUser :: Connection -> User -> Maybe User
-- insertUser = undefined