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