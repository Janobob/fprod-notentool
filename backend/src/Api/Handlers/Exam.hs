{-# LANGUAGE OverloadedStrings #-}

module Api.Handlers.Exam
    ( examHandlers
    ) where

import Servant
import Control.Monad.IO.Class (liftIO)
import Models
import Database.Core (DbPool)
import Database.Exam
import Api.Types (ExamAPI)
import Data.Int (Int64)

examHandlers :: DbPool -> Server ExamAPI
examHandlers pool = 
         getExamsHandler
    :<|> getExamHandler
    :<|> createExamHandler
    :<|> updateExamHandler
    :<|> deleteExamHandler
  where
    getExamsHandler :: Handler [ExamResponse]
    getExamsHandler = liftIO $ getExams pool

    getExamHandler :: Int64 -> Handler ExamResponse
    getExamHandler eid = do
        maybeExam <- liftIO $ getExamById pool eid
        case maybeExam of
            Just exam -> return exam
            Nothing -> throwError err404

    createExamHandler :: Exam -> Handler ExamResponse
    createExamHandler exam = liftIO $ createExam pool exam

    updateExamHandler :: Int64 -> Exam -> Handler ExamResponse
    updateExamHandler eid exam = liftIO $ updateExam pool eid exam

    deleteExamHandler :: Int64 -> Handler NoContent
    deleteExamHandler eid = do
        liftIO $ deleteExam pool eid
        return NoContent