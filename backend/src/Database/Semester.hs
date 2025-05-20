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
import Database.Persist.Sql (toSqlKey, SqlBackend, fromSqlKey)
import Data.Int (Int64)

getSemesters :: DbPool -> IO [SemesterResponse]
getSemesters pool = runDB pool $ do
    entities <- selectList [] []
    return $ map toSemesterResponse entities

getSemesterById :: DbPool -> Int64 -> IO (Maybe SemesterResponse)
getSemesterById pool sid = runDB pool $ do
    let key = toSqlKey sid :: Key Semester
    maybeEntity <- getEntity key
    return $ fmap toSemesterResponse maybeEntity

createSemester :: DbPool -> Semester -> IO SemesterResponse
createSemester pool semester = runDB pool $ do
    key <- insert semester
    return $ MkSemesterResponse (fromSqlKey key) (semesterName semester)

updateSemester :: DbPool -> Int64 -> Semester -> IO SemesterResponse
updateSemester pool sid semester = runDB pool $ do
    let semesterKey = toSqlKey sid :: Key Semester
    update semesterKey
           [SemesterName =. semesterName semester]
    return $ MkSemesterResponse sid (semesterName semester)

deleteSemester :: DbPool -> Int64 -> IO ()
deleteSemester pool sid = runDB pool $ do
    let semesterKey = toSqlKey sid :: Key Semester
    delete semesterKey

getModulesForSemester :: DbPool -> Int64 -> IO [Module]
getModulesForSemester pool sid = runDB pool $ do
    entities <- selectList [ModuleSemesterId ==. fromIntegral sid] []
    return $ map entityVal entities