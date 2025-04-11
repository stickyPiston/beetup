module Utils.Endpoint where

import Control.Monad.Reader (ReaderT, liftIO)
import Web.Twain (ResponderM)
import Database.Persist.Sql (runSqlPersistMPool, SqlBackend)
import Control.Monad.Logger (NoLoggingT)
import Conduit (ResourceT)
import Data.Pool (Pool)

type DBPool = Pool SqlBackend

type SqlQuery a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a

withDB :: DBPool -> SqlQuery a -> ResponderM a
withDB pool sql = liftIO $ runSqlPersistMPool sql pool