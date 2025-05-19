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

module Models where

import Database.Persist.TH
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Semester
    semesterId Int
    semesterName Text
    UniqueSemesterId semesterId
    deriving Show Generic

Module
    moduleId Int
    moduleName Text
    moduleAbbrevation Text
    moduleSemesterId Int
    UniqueModuleId moduleId
    deriving Show Generic

Exam
    examId Int
    examName Text
    examGrade Double
    examWeight Double
    examModuleId Int
    UniqueExamId examId
    deriving Show Generic
|]

instance ToJSON Semester
instance FromJSON Semester

instance ToJSON Module
instance FromJSON Module

instance ToJSON Exam
instance FromJSON Exam