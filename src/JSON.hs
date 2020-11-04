{-# LANGUAGE OverloadedStrings, DeriveGeneric, MultiParamTypeClasses #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module JSON where
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L 
import Types
import System.IO
import Data.Aeson
import GHC.Generics
import Control.Monad
data Content = Content {
	students :: [Student],
	attendences :: [Attendance]
} deriving (Generic,Eq,Show)

instance ToJSON Content where
	toEncoding = genericToEncoding defaultOptions

instance FromJSON Content
 
data Connection = Connection {
	fileLocation :: String
} deriving (Eq, Show)


removeFromStudentList :: StudentId -> [Student] -> [Student]
removeFromStudentList idx (x@(Student idx1 _ _ _):xs) = case idx == idx1 of
							True -> xs
							False -> x : (removeFromStudentList idx xs)
removeFromStudentList _ [] = []  

getFromStudentList :: StudentId -> [Student] -> Maybe Student
getFromStudentList idx (student@(Student idx1 _ _ _):xs) = case idx == idx1 of
							True -> Just student
							False -> getFromStudentList idx xs
getFromStudentList _ [] = Nothing

updateFromStudentList :: Student -> [Student] -> [Student]
updateFromStudentList st@(Student idx _ _ _) (x@(Student idx1 _ _ _):xs) = case idx == idx1 of
							True -> st:xs
							False -> x : (updateFromStudentList st xs)
updateFromStudentList _ [] = []

removeFromAttendanceList :: AttendanceId -> [Attendance] -> [Attendance]
removeFromAttendanceList idx (x@(Attendance idx1 _ _ _ _):xs) = case idx ==idx1 of
							True -> xs
							False -> x : (removeFromAttendanceList idx xs)
removeFromAttendanceList _ [] = []

getFromAttendanceList :: AttendanceId -> [Attendance] -> Maybe Attendance
getFromAttendanceList idx (attendance@(Attendance idx1 _ _ _ _):xs) = case idx == idx1 of
							True -> Just attendance
							False -> getFromAttendanceList idx xs
getFromAttendanceList _ [] = Nothing

updateFromAttendanceList :: Attendance -> [Attendance] -> [Attendance]
updateFromAttendanceList attd@(Attendance idx _ _ _ _) (x@(Attendance idx1 _ _ _ _):xs) = case idx==idx1 of
							True -> attd:xs
							False -> x : (updateFromAttendanceList attd xs)
updateFromAttendanceList _ [] = []



insertStudentInContent :: Student -> Content -> Content
insertStudentInContent student (Content studs attds) = Content (student:studs) attds 
removeStudentFromContent :: StudentId -> Content ->Content
removeStudentFromContent studID (Content studs attds )= Content (removeFromStudentList studID studs) attds
updateStudentFromContent :: Student -> Content -> Content
updateStudentFromContent student (Content studs attds) = Content (updateFromStudentList student studs) attds
getStudentFromContent :: StudentId -> Content -> Maybe Student
getStudentFromContent studID (Content studs attds) = getFromStudentList studID studs

insertAttendanceInContent :: Attendance -> Content -> Content
insertAttendanceInContent attendance (Content studs attds) = Content studs (attendance:attds)
removeAttendanceFromContent :: AttendanceId -> Content -> Content
removeAttendanceFromContent attID (Content studs attds) = Content studs (removeFromAttendanceList attID attds)
updateAttendanceFromContent :: Attendance -> Content -> Content
updateAttendanceFromContent attendance (Content studs attds) = Content studs (updateFromAttendanceList attendance attds)
getAttendanceFromContent :: AttendanceId -> Content -> Maybe Attendance
getAttendanceFromContent attID (Content studs attds) = getFromAttendanceList attID attds 

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
                                   

instance ModelAPI Connection IO where

	createStudent student conn = modifyDatabase insertStudentInContent student conn
        removeStudent studentID conn = modifyDatabase removeStudentFromContent studentID conn
	updateStudent student conn = modifyDatabase updateStudentFromContent student conn
        findStudent studentID conn = extractFromDatabase getStudentFromContent studentID conn 
        createAttendance attendance conn= modifyDatabase insertAttendanceInContent attendance conn
        removeAttendance attendanceID conn= modifyDatabase removeAttendanceFromContent attendanceID conn
        updateAttendance attendance conn = modifyDatabase updateAttendanceFromContent attendance conn
        findAttendance attendanceID conn =  extractFromDatabase getAttendanceFromContent attendanceID conn
