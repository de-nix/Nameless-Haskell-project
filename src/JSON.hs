{-# LANGUAGE OverloadedStrings, DeriveGeneric, MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module JSON where
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L 
import Types
import Data.Time
import System.IO
import Data.Aeson
import GHC.Generics
import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Tuple

instance Ord DayOfWeek where
  compare day1 day2 = compare (fromEnum day1) (fromEnum day2)

data Content = Content {
    students :: [Student],
    attendances :: [Attendance],
    startingTime :: UTCTime,
    timetable :: Map.Map (DayOfWeek, Int) String 
    } deriving (Generic,Show)
setStudents :: [Student] -> Content -> Content
setStudents std (Content _ att st tt) = Content std att st tt
setAttendances :: [Attendance] -> Content -> Content
setAttendances att (Content std _ st tt) = Content std att st tt
setStartingTimeImpl :: UTCTime -> Content -> Content
setStartingTimeImpl st (Content std att _ tt) = Content std att st tt
setTimetableImpl :: Map.Map (DayOfWeek,Int) String -> Content -> Content
setTimetableImpl tt (Content std att st _) = Content std att st tt

data Connection = Connection {
    fileLocation :: String
    } deriving (Show)
instance ToJSON Content where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Content
instance ModelAPI Connection IO where
    createStudent student conn = modifyDatabase createStudentImpl student conn
    removeStudent studentID conn = modifyDatabase removeStudentImpl studentID conn
    updateStudent student conn = modifyDatabase updateStudentImpl student conn
    findStudent studentID conn = extractFromDatabase findStudentImpl studentID conn
    getAllStudents conn = extractStudents conn
    createAttendance attendance conn = modifyDatabase insertAttendanceImpl attendance conn
    removeAttendance attendanceID conn= modifyDatabase removeAttendanceImpl attendanceID conn
    updateAttendance attendance conn = modifyDatabase updateAttendanceImpl attendance conn
    findAttendance attendanceID conn =  extractFromDatabase findAttendanceImpl attendanceID conn
    getSeminar time conn = getSeminarImpl time conn
    getGroup time conn = getGroupImpl time conn
    setStartingTime time conn = modifyDatabase setStartingTimeImpl time conn
    setTimetable timetable conn = modifyDatabase setTimetableImpl timetable conn

replaceStudent :: Student -> Student -> Student
replaceStudent st1@Student{Types.id = id1} st2@Student{Types.id = id2}
    | id1 == id2 = st2
    | otherwise = st1
replaceAttendance :: Attendance -> Attendance -> Attendance
replaceAttendance att1@Attendance{attendance_id = id1} att2@Attendance {attendance_id = id2}
    | id1 == id2 = att2
    | otherwise = att1
createStudentImpl    :: Student      -> Content -> Content
createStudentImpl student content@Content{students = std} = setStudents (student:std) content
removeStudentImpl    :: StudentId    -> Content -> Content
removeStudentImpl studID content@Content{students = std} = setStudents (filter (\x -> (Types.id x) /= studID ) std) content
updateStudentImpl    :: Student      -> Content -> Content
updateStudentImpl student content@Content{students = std} = setStudents (map (\x -> replaceStudent x student ) std) content
findStudentImpl      :: StudentId    -> Content -> Maybe Student
findStudentImpl studID Content{students = std}    = find (\x -> (Types.id x) == studID ) std
insertAttendanceImpl :: Attendance   -> Content -> Content
insertAttendanceImpl attendance content@Content{attendances = att} = setAttendances (attendance : att) content
removeAttendanceImpl :: AttendanceId -> Content -> Content
removeAttendanceImpl attID content@Content{attendances = att} = setAttendances (filter (\x -> (Types.attendance_id x) /= attID ) att) content
updateAttendanceImpl :: Attendance   -> Content -> Content
updateAttendanceImpl attendance content@Content{attendances = att} = setAttendances ( map (\x -> replaceAttendance x attendance) att ) content
findAttendanceImpl :: AttendanceId   -> Content -> Maybe Attendance
findAttendanceImpl attID Content{attendances = att} = find (\x -> (Types.attendance_id x) == attID ) att




getSeminarImpl ::UTCTime -> Connection -> IO Int
getSeminarImpl time (Connection x) = do
    maybeIOContent <- decode <$> (L.fromStrict<$>(S.readFile x))  
    date <- getCurrentTime 
    return $ ( div (floor(diffUTCTime  (maybe date  (\Content{ startingTime = sDate} -> sDate) maybeIOContent) time) ) (60*60*24*7)) + 1
    
getGroupImpl ::UTCTime -> Connection -> IO String
getGroupImpl time (Connection x) = do
    maybeIOContent <- decode <$> (L.fromStrict<$>(S.readFile x))
    --date <- getCurrentTime
    let timetable = maybe Map.empty (\Content{timetable = tt} -> tt) maybeIOContent
        weekDay = dayOfWeek (utctDay time)
        month = div (floor (utctDayTime time)) 3600
    return $ Map.findWithDefault "" (weekDay,month) timetable
 
modifyDatabase function argument (Connection x) = do

    maybeContent <- decode<$>( L.fromStrict <$> (S.readFile x))
    timeUTC <- getCurrentTime
    let ioContent = (maybe (Content [] [] timeUTC (Map.empty)) (function argument)) maybeContent
        ioEncoding = encode ioContent
        ioStrict = L.toStrict ioEncoding
    S.writeFile x ioStrict
    return ()

extractStudents :: Connection -> IO [Student]
extractStudents (Connection x) = do
    maybeIOContent <- decode <$> (L.fromStrict<$>(S.readFile x))
    return $ maybe [] (\c@Content{ students = std} -> std ) maybeIOContent

extractFromDatabase function argument (Connection x) = do
    maybeIOContent <- decode <$>(L.fromStrict <$> (S.readFile x))
    return $ maybe Nothing (function argument) maybeIOContent
