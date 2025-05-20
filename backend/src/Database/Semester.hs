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
import Database.Persist.Sql (toSqlKey, fromSqlKey)
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
    return $ MkSemesterResponse
        { semester_id = fromSqlKey key
        , semester_name = semesterName semester
        }

updateSemester :: DbPool -> Int64 -> Semester -> IO SemesterResponse
updateSemester pool sid semester = runDB pool $ do
    let semesterKey = toSqlKey sid :: Key Semester
    update semesterKey
           [SemesterName =. semesterName semester]
    return $ MkSemesterResponse
        { semester_id = sid
        , semester_name = semesterName semester
        }

deleteSemester :: DbPool -> Int64 -> IO ()
deleteSemester pool sid = runDB pool $ do
    let semesterKey = toSqlKey sid :: Key Semester
    delete semesterKey

getModulesForSemester :: DbPool -> Int64 -> IO [ModuleResponse]
getModulesForSemester pool sid = runDB pool $ do
    entities <- selectList [ModuleSemesterId ==. fromIntegral sid] []
    return $ map toModuleResponse entities