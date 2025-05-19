{-# LANGUAGE OverloadedStrings #-}

module Database.Exam
    ( getExams
    , getExamById
    , createExam
    , updateExam
    , deleteExam
    ) where

import Database.Persist
import Models
import Database.Core (DbPool, runDB)
import Database.Persist.Sql (toSqlKey, SqlBackend)
import Data.Int (Int64)

getExams :: DbPool -> IO [Exam]
getExams pool = runDB pool $ do
    entities <- selectList [] []
    return $ map entityVal entities

getExamById :: DbPool -> Int64 -> IO (Maybe Exam)
getExamById pool eid = runDB pool $ do
    let key = toSqlKey eid :: Key Exam
    get key

createExam :: DbPool -> Exam -> IO Exam
createExam pool exam = runDB pool $ do
    _ <- insert exam
    return exam

updateExam :: DbPool -> Int64 -> Exam -> IO Exam
updateExam pool eid exam = runDB pool $ do
    let examKey = toSqlKey eid :: Key Exam
    update examKey
          [ ExamName =. examName exam
          , ExamGrade =. examGrade exam
          , ExamWeight =. examWeight exam
          , ExamModuleId =. examModuleId exam
          ]
    return exam

deleteExam :: DbPool -> Int64 -> IO ()
deleteExam pool eid = runDB pool $ do
    let examKey = toSqlKey eid :: Key Exam
    delete examKey