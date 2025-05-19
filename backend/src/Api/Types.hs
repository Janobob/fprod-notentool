{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Types where

import Servant
import Models

type API = "api" :> (
        SemesterAPI
        :<|> ModuleAPI
        :<|> ExamAPI
    )

type SemesterAPI = 
        "semesters" :> Get '[JSON] [Semester]
        :<|> "semesters" :> Capture "semesterId" Int :> Get '[JSON] Semester
        :<|> "semesters" :> Capture "semesterId" Int :> "modules" :> Get '[JSON] [Module]
        :<|> "semesters" :> ReqBody '[JSON] Semester :> Post '[JSON] Semester
        :<|> "semesters" :> Capture "semesterId" Int :> ReqBody '[JSON] Semester :> Put '[JSON] Semester
        :<|> "semesters" :> Capture "semesterId" Int :> Delete '[JSON] NoContent

type ModuleAPI = 
        "modules" :> Get '[JSON] [Module]
        :<|> "modules" :> Capture "moduleId" Int :> Get '[JSON] Module
        :<|> "modules" :> Capture "moduleId" Int :> "exams" :> Get '[JSON] [Exam]
        :<|> "modules" :> ReqBody '[JSON] Module :> Post '[JSON] Module
        :<|> "modules" :> Capture "moduleId" Int :> ReqBody '[JSON] Module :> Put '[JSON] Module
        :<|> "modules" :> Capture "moduleId" Int :> Delete '[JSON] NoContent

type ExamAPI = 
        "exams" :> Get '[JSON] [Exam]
        :<|> "exams" :> Capture "examId" Int :> Get '[JSON] Exam
        :<|> "exams" :> ReqBody '[JSON] Exam :> Post '[JSON] Exam
        :<|> "exams" :> Capture "examId" Int :> ReqBody '[JSON] Exam :> Put '[JSON] Exam
        :<|> "exams" :> Capture "examId" Int :> Delete '[JSON] NoContent