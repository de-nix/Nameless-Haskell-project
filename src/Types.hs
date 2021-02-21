{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Types where

import qualified Control.Lens
import           Control.Lens.Operators
import           Control.Lens.TH
import           Data.Aeson
import qualified Data.Map               as Map
import           Data.Time
import           GHC.Generics

type StudentName   = String
type StudentId     = Integer
type Group         = String
type Code          = String
type AttendanceId  = Integer
type SeminarNumber = Integer

data Student = Student
    { _sId    :: StudentId
    , _sName  :: String
    , _sGroup :: Group
    , _sCode  :: Code
    } deriving (Generic,Eq, Show)
$(makeLenses ''Student)

data Attendance = Attendance 
    { _aId        :: AttendanceId
    , _aStudentId :: StudentId
    , _aSeminar   :: SeminarNumber
    , _aGroup     :: Group
    , _aActivity  :: Bool
    } deriving (Generic, Eq, Show)
$(makeLenses ''Attendance)

instance ToJSON Student where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Student

instance ToJSON Attendance where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Attendance

class ModelAPI a m where
    createStudent    :: (Monad m) => Student      -> a -> m ()
    removeStudent    :: (Monad m) => StudentId    -> a -> m ()
    updateStudent    :: (Monad m) => Student      -> a -> m ()
    findStudent      :: (Monad m) => StudentId    -> a -> m (Maybe Student)
    getAllStudents   :: (Monad m) =>                 a -> m ([Student])
    createAttendance :: (Monad m) => Attendance   -> a -> m ()
    removeAttendance :: (Monad m) => AttendanceId -> a -> m ()
    updateAttendance :: (Monad m) => Attendance   -> a -> m ()
    findAttendance   :: (Monad m) => AttendanceId -> a -> m (Maybe Attendance)
    getAllAttendances:: (Monad m) =>                 a -> m ([Attendance])
