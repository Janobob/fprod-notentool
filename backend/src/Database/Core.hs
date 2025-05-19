{-# LANGUAGE OverloadedStrings #-}

module Database.Core
    ( initializeDatabase
    , runDB
    , DbPool
    ) where

import Control.Monad.Logger (runStderrLoggingT, LoggingT)
import Control.Monad.Reader (ReaderT)
import Database.Persist.Sqlite
import Data.Pool (Pool)
import Models (migrateAll)

type DbPool = Pool SqlBackend

initializeDatabase :: IO DbPool
initializeDatabase = runStderrLoggingT $ do
    pool <- createSqlitePool "notentool.db" 10
    runSqlPool (runMigration migrateAll) pool
    return pool

runDB :: DbPool -> ReaderT SqlBackend (LoggingT IO) a -> IO a
runDB pool action = runStderrLoggingT $ runSqlPool action pool