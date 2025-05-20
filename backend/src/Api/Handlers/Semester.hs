module Api.Handlers.Semester
    ( semesterHandlers
    ) where

import Servant
import Control.Monad.IO.Class (liftIO)
import Models
import Database.Core (DbPool)
import Database.Semester
import Api.Types (SemesterAPI)
import Data.Int (Int64)

semesterHandlers :: DbPool -> Server SemesterAPI
semesterHandlers pool = 
         getSemestersHandler
    :<|> getSemesterHandler
    :<|> getModulesForSemesterHandler
    :<|> createSemesterHandler
    :<|> updateSemesterHandler
    :<|> deleteSemesterHandler
  where
    getSemestersHandler :: Handler [SemesterResponse]
    getSemestersHandler = liftIO $ getSemesters pool

    getSemesterHandler :: Int64 -> Handler SemesterResponse
    getSemesterHandler sid = do
        maybeSemester <- liftIO $ getSemesterById pool sid
        case maybeSemester of
            Just semester -> return semester
            Nothing -> throwError err404

    getModulesForSemesterHandler :: Int64 -> Handler [Module]
    getModulesForSemesterHandler sid = liftIO $ getModulesForSemester pool sidy

    createSemesterHandler :: Semester -> Handler SemesterResponse
    createSemesterHandler semester = liftIO $ createSemester pool semester

    updateSemesterHandler :: Int64 -> Semester -> Handler SemesterResponse
    updateSemesterHandler sid semester = liftIO $ updateSemester pool sid semester

    deleteSemesterHandler :: Int64 -> Handler NoContent
    deleteSemesterHandler sid = do
        liftIO $ deleteSemester pool sid
        return NoContent