{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Integration.UserStore where

import Data.Text (Text)
import Database.Persist
import Database.Persist.Sqlite (runSqlite, toSqlKey)
import Integration.Init


findUserByUsername :: Text -> IO (Maybe (Entity User))
findUserByUsername uname = runSqlite "main.db" $ selectFirst [UserUsername ==. uname] []

findUserById :: Int -> IO (Maybe (Entity User))
findUserById id = runSqlite "main.db" $ selectFirst [UserId ==. toSqlKey (fromIntegral id)] []

insertUser :: User -> IO UserId
insertUser u = runSqlite "main.db" $ insert u
