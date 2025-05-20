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
    { id :: Int64
    , name :: Text
    } deriving (Show, Generic)

instance ToJSON SemesterResponse
instance FromJSON SemesterResponse

toSemesterResponse :: Entity Semester -> SemesterResponse
toSemesterResponse (Entity key val) = MkSemesterResponse
    { id = fromSqlKey key
    , name = semesterName val
    }

-- Module Response
data ModuleResponse = MkModuleResponse
    { id :: Int64
    , name :: Text
    , abbrevation :: Text
    , semesterId :: Int64
    } deriving (Show, Generic)

instance ToJSON ModuleResponse
instance FromJSON ModuleResponse

toModuleResponse :: Entity Module -> ModuleResponse
toModuleResponse (Entity key val) = MkModuleResponse
    { id = fromSqlKey key
    , name = moduleName val
    , abbrevation = moduleAbbrevation val
    , semesterId = fromIntegral $ moduleSemesterId val
    }

-- Exam Response
data ExamResponse = MkExamResponse
    { id :: Int64
    , name :: Text
    , grade :: Double
    , weight :: Double
    , moduleId :: Int64
    } deriving (Show, Generic)

instance ToJSON ExamResponse
instance FromJSON ExamResponse

toExamResponse :: Entity Exam -> ExamResponse
toExamResponse (Entity key val) = MkExamResponse
    { id = fromSqlKey key
    , name = examName val
    , grade = examGrade val
    , weight = examWeight val
    , moduleId = fromIntegral $ examModuleId val
    }