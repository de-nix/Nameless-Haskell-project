{-# LANGUAGE OverloadedStrings, DeriveGeneric, MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules, NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module JSON where
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L 
import Types
import System.IO
import Data.Aeson
import GHC.Generics
import Control.Monad
import Data.List

data Content = Content {
    students :: [Student],
    attendences :: [Attendance]
    } deriving (Generic,Show)
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
    createAttendance attendance conn = modifyDatabase insertAttendanceImpl attendance conn
    removeAttendance attendanceID conn= modifyDatabase removeAttendanceImpl attendanceID conn
    updateAttendance attendance conn = modifyDatabase updateAttendanceImpl attendance conn
    findAttendance attendanceID conn =  extractFromDatabase findAttendanceImpl attendanceID conn

replaceStudent :: Student -> Student -> Student
replaceStudent st1@Student{Types.id = id1} st2@Student{Types.id = id2}
    | id1 == id2 = st2
    | otherwise = st1
replaceAttendance :: Attendance -> Attendance -> Attendance
replaceAttendance att1@Attendance{attendance_id = id1} att2@Attendance {attendance_id = id2}
    | id1 == id2 = att2
    | otherwise = att1
createStudentImpl    :: Student      -> Content -> Content
createStudentImpl student (Content studs attds) = Content (student:studs) attds 
removeStudentImpl    :: StudentId    -> Content -> Content
removeStudentImpl studID (Content studs attds ) = Content (filter (\x -> (Types.id x) /= studID ) studs) attds
updateStudentImpl    :: Student      -> Content -> Content
updateStudentImpl student (Content studs attds) = Content (map (\x -> replaceStudent x student ) studs) attds
findStudentImpl      :: StudentId    -> Content -> Maybe Student
findStudentImpl studID (Content studs attds)    = find (\x -> (Types.id x) == studID ) studs
insertAttendanceImpl :: Attendance   -> Content -> Content
insertAttendanceImpl attendance (Content studs attds) = Content studs (attendance:attds)
removeAttendanceImpl :: AttendanceId -> Content -> Content
removeAttendanceImpl attID (Content studs attds) = Content studs (filter (\x -> (Types.attendance_id x) /= attID ) attds)
updateAttendanceImpl :: Attendance   -> Content -> Content
updateAttendanceImpl attendance (Content studs attds) = Content studs ( map (\x -> replaceAttendance x attendance) attds)
findAttendanceImpl :: AttendanceId   -> Content -> Maybe Attendance
findAttendanceImpl attID (Content studs attds) = find (\x -> (Types.attendance_id x) == attID ) attds 
modifyDatabase function argument (Connection x) = do
    maybeContent <- decode<$>( L.fromStrict <$> (S.readFile x))
    let ioContent = (maybe (Content [] []) (function argument)) maybeContent
        ioEncoding = encode ioContent
        ioStrict = L.toStrict ioEncoding
    S.writeFile x ioStrict
    return ()
extractFromDatabase function argument (Connection x) = do
    maybeIOContent <- decode <$>(L.fromStrict <$> (S.readFile x))
    return $ maybe Nothing (function argument) maybeIOContent
