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

getModules :: DbPool -> IO [Module]
getModules pool = runDB pool $ do
    entities <- selectList [] []
    return $ map entityVal entities

getModuleById :: DbPool -> Int -> IO (Maybe Module)
getModuleById pool mid = runDB pool $ do
    entity <- getBy $ UniqueModuleId mid
    return $ fmap entityVal entity

createModule :: DbPool -> Module -> IO Module
createModule pool module' = runDB pool $ do
    _ <- insert module'
    return module'

updateModule :: DbPool -> Int -> Module -> IO Module
updateModule pool mid module' = runDB pool $ do
    _ <- updateWhere [ModuleModuleId ==. mid]
                    [ ModuleModuleName =. moduleModuleName module'
                    , ModuleModuleAbbrevation =. moduleModuleAbbrevation module'
                    , ModuleModuleSemesterId =. moduleModuleSemesterId module'
                    ]
    return module'

deleteModule :: DbPool -> Int -> IO ()
deleteModule pool mid = runDB pool $
    deleteWhere [ModuleModuleId ==. mid]

getExamsForModule :: DbPool -> Int -> IO [Exam]
getExamsForModule pool mid = runDB pool $ do
    entities <- selectList [ExamExamModuleId ==. mid] []
    return $ map entityVal entities