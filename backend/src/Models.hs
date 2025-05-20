{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models where

import Database.Persist.TH
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Int (Int64)
import Database.Persist (Entity(..))
import Database.Persist.Sql (fromSqlKey)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Semester
    name Text
    deriving Show Generic

Module
    name Text
    abbrevation Text
    semesterId Int
    deriving Show Generic

Exam
    name Text
    grade Double
    weight Double
    moduleId Int
    deriving Show Generic
|]

instance ToJSON Semester
instance FromJSON Semester

instance ToJSON Module
instance FromJSON Module

instance ToJSON Exam
instance FromJSON Exam

-- Semester Response
data SemesterResponse = MkSemesterResponse
    { semester_id :: Int64
    , semester_name :: Text
    } deriving (Show, Generic)

instance ToJSON SemesterResponse
instance FromJSON SemesterResponse

toSemesterResponse :: Entity Semester -> SemesterResponse
toSemesterResponse (Entity key val) = MkSemesterResponse
    { semester_id = fromSqlKey key
    , semester_name = semesterName val
    }

-- Module Response
data ModuleResponse = MkModuleResponse
    { module_id :: Int64
    , module_name :: Text
    , module_abbrevation :: Text
    , module_semesterId :: Int64
    } deriving (Show, Generic)

instance ToJSON ModuleResponse
instance FromJSON ModuleResponse

toModuleResponse :: Entity Module -> ModuleResponse
toModuleResponse (Entity key val) = MkModuleResponse
    { module_id = fromSqlKey key
    , module_name = moduleName val
    , module_abbrevation = moduleAbbrevation val
    , module_semesterId = fromIntegral $ moduleSemesterId val
    }

-- Exam Response
data ExamResponse = MkExamResponse
    { exam_id :: Int64
    , exam_name :: Text
    , exam_grade :: Double
    , exam_weight :: Double
    , exam_moduleId :: Int64
    } deriving (Show, Generic)

instance ToJSON ExamResponse
instance FromJSON ExamResponse

toExamResponse :: Entity Exam -> ExamResponse
toExamResponse (Entity key val) = MkExamResponse
    { exam_id = fromSqlKey key
    , exam_name = examName val
    , exam_grade = examGrade val
    , exam_weight = examWeight val
    , exam_moduleId = fromIntegral $ examModuleId val
    }