{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api(API, api, server) where

import Servant
import Models

type API = "api" 
    :>   "semesters" :> Get '[JSON] [Semester]
    :<|> "semesters" :> Capture "semesterId" Int :> Get '[JSON] Semester
    :<|> "semesters" :> Capture "semesterId" Int :> "modules" :> Get '[JSON] [Module]
    :<|> "semesters" :> ReqBody '[JSON] Semester :> Post '[JSON] Semester
    :<|> "semesters" :> Capture "semesterId" Int :> ReqBody '[JSON] Semester :> Put '[JSON] Semester
    :<|> "semesters" :> Capture "semesterId" Int :> Delete '[JSON] NoContent

    :<|> "modules" :> Get '[JSON] [Module]
    :<|> "modules" :> Capture "moduleId" Int :> Get '[JSON] Module
    :<|> "modules" :> Capture "moduleId" Int :> "exams" :> Get '[JSON] [Exam]
    :<|> "modules" :> ReqBody '[JSON] Module :> Post '[JSON] Module
    :<|> "modules" :> Capture "moduleId" Int :> ReqBody '[JSON] Module :> Put '[JSON] Module
    :<|> "modules" :> Capture "moduleId" Int :> Delete '[JSON] NoContent

    :<|> "exams" :> Get '[JSON] [Exam]
    :<|> "exams" :> Capture "examId" Int :> Get '[JSON] Exam
    :<|> "exams" :> ReqBody '[JSON] Exam :> Post '[JSON] Exam
    :<|> "exams" :> Capture "examId" Int :> ReqBody '[JSON] Exam :> Put '[JSON] Exam
    :<|> "exams" :> Capture "examId" Int :> Delete '[JSON] NoContent

api :: Proxy API
api = Proxy

server :: Server API
server = 
         getSemesters
    :<|> getSemester
    :<|> getModulesForSemester
    :<|> createSemester
    :<|> updateSemester
    :<|> deleteSemester
    :<|> getModules
    :<|> getModule
    :<|> getExamsForModule
    :<|> createModule
    :<|> updateModule
    :<|> deleteModule
    :<|> getExams
    :<|> getExam
    :<|> createExam
    :<|> updateExam
    :<|> deleteExam

-- currently all handlers return dummy data
-- waiting for the database to be set up
getSemesters :: Handler [Semester]
getSemesters = return [Semester 1 "HS25", Semester 2 "FS25"]

getSemester :: Int -> Handler Semester
getSemester semesterId = return $ Semester semesterId "HS25"

getModulesForSemester :: Int -> Handler [Module]
getModulesForSemester semesterId = return [Module 1 "fprog" "Funktionales Programmieren" semesterId, Module 2 "fprod" "Funktionales Programming Design" semesterId]

createSemester :: Semester -> Handler Semester
createSemester semester = return semester

updateSemester :: Int -> Semester -> Handler Semester
updateSemester semesterId semester = return semester

deleteSemester :: Int -> Handler NoContent
deleteSemester semesterId = return NoContent

getModules :: Handler [Module]
getModules = return [Module 1 "fprog" "Funktionales Programmieren" 1, Module 2 "fprod" "Funktionales Programming Design" 1]

getModule :: Int -> Handler Module
getModule moduleId = return $ Module moduleId "fprog" "Funktionales Programmieren" 1

getExamsForModule :: Int -> Handler [Exam]
getExamsForModule moduleId = return [Exam 1 "Assessment 1" 5.2 0.5 moduleId, Exam 2 "Projekt" 6.0 0.5 moduleId]

createModule :: Module -> Handler Module
createModule module' = return module'

updateModule :: Int -> Module -> Handler Module
updateModule moduleId module' = return module'

deleteModule :: Int -> Handler NoContent
deleteModule moduleId = return NoContent

getExams :: Handler [Exam]
getExams = return [Exam 1 "Assessment 1" 5.2 0.5 1, Exam 2 "Projekt" 6.0 0.5 1]

getExam :: Int -> Handler Exam
getExam examId = return $ Exam examId "Assessment 1" 5.2 0.5 1

createExam :: Exam -> Handler Exam
createExam exam = return exam

updateExam :: Int -> Exam -> Handler Exam
updateExam examId exam = return exam

deleteExam :: Int -> Handler NoContent
deleteExam examId = return NoContent