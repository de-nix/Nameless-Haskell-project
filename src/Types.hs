{-# LANGUAGE RankNTypes, MultiParamTypeClasses, OverloadedStrings, DeriveGeneric #-}
module Types where
import Data.Aeson
import GHC.Generics
type StudentName = String
type StudentId = Integer
type Group = String
type Code = String
type AttendanceId = Integer
type SeminarNumber = Integer

data Student = Student{
	id :: StudentId,
	name :: String,
	student_group :: Group,
	code :: Code } deriving (Generic,Eq, Show)

data Attendance = Attendance {
	
        attendance_id :: AttendanceId,
        studentId :: StudentId,
	seminar :: SeminarNumber,
        group :: Group,
	activity :: Bool
} deriving (Generic, Eq, Show)


instance ToJSON Student where
     toEncoding = genericToEncoding defaultOptions
instance FromJSON Student

instance ToJSON Attendance where
	toEncoding = genericToEncoding defaultOptions
instance FromJSON Attendance

class ModelAPI a m where
	createStudent :: (Monad m) => Student   -> a -> m ()
	removeStudent :: (Monad m) => StudentId -> a -> m ()
	updateStudent :: (Monad m) => Student   -> a -> m ()
	findStudent   :: (Monad m) => StudentId -> a -> m (Maybe Student)

	createAttendance :: (Monad m) => Attendance   -> a -> m ()
	removeAttendance :: (Monad m) => AttendanceId -> a -> m ()
        updateAttendance :: (Monad m) => Attendance   -> a -> m ()
        findAttendance   :: (Monad m) => AttendanceId -> a -> m (Maybe Attendance) 


toggleAttendance ::(Monad m, ModelAPI a m) => a -> Attendance -> m ()

toggleAttendance conn attendance = do
	maybeAttendance <- findAttendance (attendance_id attendance) conn  
	case maybeAttendance of
		Nothing -> createAttendance attendance conn
		Just att -> removeAttendance (attendance_id att) conn
	
	




