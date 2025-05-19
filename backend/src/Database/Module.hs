{-# LANGUAGE OverloadedStrings #-}

module Database.Module
    ( getModules
    , getModuleById
    , createModule
    , updateModule
    , deleteModule
    , getExamsForModule
    ) where

import Database.Persist
import Models
import Database.Core (DbPool, runDB)
import Database.Persist.Sql (toSqlKey, SqlBackend)
import Data.Int (Int64)

getModules :: DbPool -> IO [Module]
getModules pool = runDB pool $ do
    entities <- selectList [] []
    return $ map entityVal entities

getModuleById :: DbPool -> Int64 -> IO (Maybe Module)
getModuleById pool mid = runDB pool $ do
    let key = toSqlKey mid :: Key Module
    get key

createModule :: DbPool -> Module -> IO Module
createModule pool module' = runDB pool $ do
    _ <- insert module'
    return module'

updateModule :: DbPool -> Int64 -> Module -> IO Module
updateModule pool mid module' = runDB pool $ do
    let moduleKey = toSqlKey mid :: Key Module
    update moduleKey
           [ ModuleName =. moduleName module'
           , ModuleAbbrevation =. moduleAbbrevation module'
           , ModuleSemesterId =. moduleSemesterId module'
           ]
    return module'

deleteModule :: DbPool -> Int64 -> IO ()
deleteModule pool mid = runDB pool $ do
    let moduleKey = toSqlKey mid :: Key Module
    delete moduleKey

getExamsForModule :: DbPool -> Int64 -> IO [Exam]
getExamsForModule pool mid = runDB pool $ do
    entities <- selectList [ExamModuleId ==. fromIntegral mid] []
    return $ map entityVal entities