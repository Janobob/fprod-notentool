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
import Database.Persist.Sql (toSqlKey, fromSqlKey)
import Data.Int (Int64)
import Prelude hiding (id)

getExams :: DbPool -> IO [ExamResponse]
getExams pool = runDB pool $ do
    entities <- selectList [] []
    return $ map toExamResponse entities

getExamById :: DbPool -> Int64 -> IO (Maybe ExamResponse)
getExamById pool eid = runDB pool $ do
    let key = toSqlKey eid :: Key Exam
    maybeEntity <- getEntity key
    return $ fmap toExamResponse maybeEntity

createExam :: DbPool -> Exam -> IO ExamResponse
createExam pool exam = runDB pool $ do
    key <- insert exam
    return $ MkExamResponse
        { exam_id = fromSqlKey key
        , exam_name = examName exam
        , exam_grade = examGrade exam
        , exam_weight = examWeight exam
        , exam_moduleId = fromIntegral $ examModuleId exam
        }

updateExam :: DbPool -> Int64 -> Exam -> IO ExamResponse
updateExam pool eid exam = runDB pool $ do
    let examKey = toSqlKey eid :: Key Exam
    update examKey
          [ ExamName =. examName exam
          , ExamGrade =. examGrade exam
          , ExamWeight =. examWeight exam
          , ExamModuleId =. examModuleId exam
          ]
    return $ MkExamResponse
        { exam_id = eid
        , exam_name = examName exam
        , exam_grade = examGrade exam
        , exam_weight = examWeight exam
        , exam_moduleId = fromIntegral $ examModuleId exam
        }

deleteExam :: DbPool -> Int64 -> IO ()
deleteExam pool eid = runDB pool $ do
    let examKey = toSqlKey eid :: Key Exam
    delete examKey