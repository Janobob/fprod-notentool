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
import Database.Persist.Sql (toSqlKey, SqlBackend)
import Data.Int (Int64)

getSemesters :: DbPool -> IO [Semester]
getSemesters pool = runDB pool $ do
    entities <- selectList [] []
    return $ map entityVal entities

getSemesterById :: DbPool -> Int64 -> IO (Maybe Semester)
getSemesterById pool sid = runDB pool $ do
    let key = toSqlKey sid :: Key Semester
    get key

createSemester :: DbPool -> Semester -> IO Semester
createSemester pool semester = runDB pool $ do
    _ <- insert semester
    return semester

updateSemester :: DbPool -> Int64 -> Semester -> IO Semester
updateSemester pool sid semester = runDB pool $ do
    let semesterKey = toSqlKey sid :: Key Semester
    update semesterKey
           [SemesterName =. semesterName semester]
    return semester

deleteSemester :: DbPool -> Int64 -> IO ()
deleteSemester pool sid = runDB pool $ do
    let semesterKey = toSqlKey sid :: Key Semester
    delete semesterKey

getModulesForSemester :: DbPool -> Int64 -> IO [Module]
getModulesForSemester pool sid = runDB pool $ do
    entities <- selectList [ModuleSemesterId ==. fromIntegral sid] []
    return $ map entityVal entities