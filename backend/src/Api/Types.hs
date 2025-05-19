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
        "semesters" :> Get '[JSON] [Semester]
        :<|> "semesters" :> Capture "semesterId" Int64 :> Get '[JSON] Semester
        :<|> "semesters" :> Capture "semesterId" Int64 :> "modules" :> Get '[JSON] [Module]
        :<|> "semesters" :> ReqBody '[JSON] Semester :> Post '[JSON] Semester
        :<|> "semesters" :> Capture "semesterId" Int64 :> ReqBody '[JSON] Semester :> Put '[JSON] Semester
        :<|> "semesters" :> Capture "semesterId" Int64 :> Delete '[JSON] NoContent

type ModuleAPI = 
        "modules" :> Get '[JSON] [Module]
        :<|> "modules" :> Capture "moduleId" Int64 :> Get '[JSON] Module
        :<|> "modules" :> Capture "moduleId" Int64 :> "exams" :> Get '[JSON] [Exam]
        :<|> "modules" :> ReqBody '[JSON] Module :> Post '[JSON] Module
        :<|> "modules" :> Capture "moduleId" Int64 :> ReqBody '[JSON] Module :> Put '[JSON] Module
        :<|> "modules" :> Capture "moduleId" Int64 :> Delete '[JSON] NoContent

type ExamAPI = 
        "exams" :> Get '[JSON] [Exam]
        :<|> "exams" :> Capture "examId" Int64 :> Get '[JSON] Exam
        :<|> "exams" :> ReqBody '[JSON] Exam :> Post '[JSON] Exam
        :<|> "exams" :> Capture "examId" Int64 :> ReqBody '[JSON] Exam :> Put '[JSON] Exam
        :<|> "exams" :> Capture "examId" Int64 :> Delete '[JSON] NoContent