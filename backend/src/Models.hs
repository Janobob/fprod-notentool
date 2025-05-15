{-# LANGUAGE DeriveGeneric #-}

module Models where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)

data Semester = Semester
  { semesterId   :: Int
  , semesterName :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON Semester
instance FromJSON Semester

data Module = Module
  { moduleId         :: Int
  , moduleName       :: Text
  , moduleSemesterId :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Module
instance FromJSON Module

data Exam = Exam
  { examId        :: Int
  , examName      :: Text
  , examGrade     :: Double
  , examWeight    :: Double
  , examModuleId  :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON Exam
instance FromJSON Exam