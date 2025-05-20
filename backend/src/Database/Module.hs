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
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Data.Int (Int64)

getModules :: DbPool -> IO [ModuleResponse]
getModules pool = runDB pool $ do
    entities <- selectList [] []
    return $ map toModuleResponse entities

getModuleById :: DbPool -> Int64 -> IO (Maybe ModuleResponse)
getModuleById pool mid = runDB pool $ do
    let key = toSqlKey mid :: Key Module
    maybeEntity <- getEntity key
    return $ fmap toModuleResponse maybeEntity

createModule :: DbPool -> Module -> IO ModuleResponse
createModule pool module' = runDB pool $ do
    key <- insert module'
    return $ MkModuleResponse
        { module_id = fromSqlKey key
        , module_name = moduleName module'
        , module_abbrevation = moduleAbbrevation module'
        , module_semesterId = fromIntegral $ moduleSemesterId module'
        }

updateModule :: DbPool -> Int64 -> Module -> IO ModuleResponse
updateModule pool mid module' = runDB pool $ do
    let moduleKey = toSqlKey mid :: Key Module
    update moduleKey
           [ ModuleName =. moduleName module'
           , ModuleAbbrevation =. moduleAbbrevation module'
           , ModuleSemesterId =. moduleSemesterId module'
           ]
    return $ MkModuleResponse 
        { module_id = mid
        , module_name = moduleName module'
        , module_abbrevation = moduleAbbrevation module'
        , module_semesterId = fromIntegral $ moduleSemesterId module'
        }

deleteModule :: DbPool -> Int64 -> IO ()
deleteModule pool mid = runDB pool $ do
    let moduleKey = toSqlKey mid :: Key Module
    delete moduleKey

getExamsForModule :: DbPool -> Int64 -> IO [ExamResponse]
getExamsForModule pool mid = runDB pool $ do
    entities <- selectList [ExamModuleId ==. fromIntegral mid] []
    return $ map toExamResponse entities