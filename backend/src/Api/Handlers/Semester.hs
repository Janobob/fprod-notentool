{-# LANGUAGE OverloadedStrings #-}

module Api.Handlers.Semester
    ( semesterHandlers
    ) where

import Servant
import Control.Monad.IO.Class (liftIO)
import Models
import Database.Core (DbPool)
import Database.Semester
import Api.Types (SemesterAPI)

semesterHandlers :: DbPool -> Server SemesterAPI
semesterHandlers pool = 
         getSemestersHandler
    :<|> getSemesterHandler
    :<|> getModulesForSemesterHandler
    :<|> createSemesterHandler
    :<|> updateSemesterHandler
    :<|> deleteSemesterHandler
  where
    getSemestersHandler :: Handler [Semester]
    getSemestersHandler = liftIO $ getSemesters pool

    getSemesterHandler :: Int -> Handler Semester
    getSemesterHandler sid = do
        maybeSemester <- liftIO $ getSemesterById pool sid
        case maybeSemester of
            Just semester -> return semester
            Nothing -> throwError err404

    getModulesForSemesterHandler :: Int -> Handler [Module]
    getModulesForSemesterHandler sid = liftIO $ getModulesForSemester pool sid

    createSemesterHandler :: Semester -> Handler Semester
    createSemesterHandler semester = liftIO $ createSemester pool semester

    updateSemesterHandler :: Int -> Semester -> Handler Semester
    updateSemesterHandler sid semester = liftIO $ updateSemester pool sid semester

    deleteSemesterHandler :: Int -> Handler NoContent
    deleteSemesterHandler sid = do
        liftIO $ deleteSemester pool sid
        return NoContent