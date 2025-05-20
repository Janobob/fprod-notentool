{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Types where

import Servant
import Models
import Data.Int (Int64)

type API = "api" :> (
        SemesterAPI
        :<|> ModuleAPI
        :<|> ExamAPI
    )

type SemesterAPI = 
        "semesters" :> Get '[JSON] [SemesterResponse]
        :<|> "semesters" :> Capture "semesterId" Int64 :> Get '[JSON] SemesterResponse
        :<|> "semesters" :> Capture "semesterId" Int64 :> "modules" :> Get '[JSON] [ModuleResponse]
        :<|> "semesters" :> ReqBody '[JSON] Semester :> Post '[JSON] SemesterResponse
        :<|> "semesters" :> Capture "semesterId" Int64 :> ReqBody '[JSON] Semester :> Put '[JSON] SemesterResponse
        :<|> "semesters" :> Capture "semesterId" Int64 :> Delete '[JSON] NoContent

type ModuleAPI = 
        "modules" :> Get '[JSON] [ModuleResponse]
        :<|> "modules" :> Capture "moduleId" Int64 :> Get '[JSON] ModuleResponse
        :<|> "modules" :> Capture "moduleId" Int64 :> "exams" :> Get '[JSON] [ExamResponse]
        :<|> "modules" :> ReqBody '[JSON] Module :> Post '[JSON] ModuleResponse
        :<|> "modules" :> Capture "moduleId" Int64 :> ReqBody '[JSON] Module :> Put '[JSON] ModuleResponse
        :<|> "modules" :> Capture "moduleId" Int64 :> Delete '[JSON] NoContent

type ExamAPI = 
        "exams" :> Get '[JSON] [ExamResponse]
        :<|> "exams" :> Capture "examId" Int64 :> Get '[JSON] ExamResponse
        :<|> "exams" :> ReqBody '[JSON] Exam :> Post '[JSON] ExamResponse
        :<|> "exams" :> Capture "examId" Int64 :> ReqBody '[JSON] Exam :> Put '[JSON] ExamResponse
        :<|> "exams" :> Capture "examId" Int64 :> Delete '[JSON] NoContent