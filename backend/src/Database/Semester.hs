{-# LANGUAGE OverloadedStrings #-}

module Database.Semester
    ( getSemesters
    , getSemesterById
    , createSemester
    , updateSemester
    , deleteSemester
    , getModulesForSemester
    ) where

import Database.Persist
import Models
import Database.Core (DbPool, runDB)

getSemesters :: DbPool -> IO [Semester]
getSemesters pool = runDB pool $ do
    entities <- selectList [] []
    return $ map entityVal entities

getSemesterById :: DbPool -> Int -> IO (Maybe Semester)
getSemesterById pool sid = runDB pool $ do
    entity <- getBy $ UniqueSemesterId sid
    return $ fmap entityVal entity

createSemester :: DbPool -> Semester -> IO Semester
createSemester pool semester = runDB pool $ do
    _ <- insert semester
    return semester

updateSemester :: DbPool -> Int -> Semester -> IO Semester
updateSemester pool sid semester = runDB pool $ do
    _ <- updateWhere [SemesterSemesterId ==. sid] 
                    [SemesterSemesterName =. semesterSemesterName semester]
    return semester

deleteSemester :: DbPool -> Int -> IO ()
deleteSemester pool sid = runDB pool $ 
    deleteWhere [SemesterSemesterId ==. sid]

getModulesForSemester :: DbPool -> Int -> IO [Module]
getModulesForSemester pool sid = runDB pool $ do
    entities <- selectList [ModuleModuleSemesterId ==. sid] []
    return $ map entityVal entities
