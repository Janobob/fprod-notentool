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

getExams :: DbPool -> IO [Exam]
getExams pool = runDB pool $ do
    entities <- selectList [] []
    return $ map entityVal entities

getExamById :: DbPool -> Int -> IO (Maybe Exam)
getExamById pool eid = runDB pool $ do
    entity <- getBy $ UniqueExamId eid
    return $ fmap entityVal entity

createExam :: DbPool -> Exam -> IO Exam
createExam pool exam = runDB pool $ do
    _ <- insert exam
    return exam

updateExam :: DbPool -> Int -> Exam -> IO Exam
updateExam pool eid exam = runDB pool $ do
    _ <- updateWhere [ExamExamId ==. eid]
                    [ ExamExamName =. examExamName exam
                    , ExamExamGrade =. examExamGrade exam
                    , ExamExamWeight =. examExamWeight exam
                    , ExamExamModuleId =. examExamModuleId exam
                    ]
    return exam

deleteExam :: DbPool -> Int -> IO ()
deleteExam pool eid = runDB pool $
    deleteWhere [ExamExamId ==. eid]