{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE InstanceSigs       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module JSON where

import Control.Lens (set)
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad

import           Data.Aeson
import qualified Data.ByteString        as S
import qualified Data.ByteString.Lazy   as L
import           Data.List
import qualified Data.Map               as Map
import           Data.Time
import Data.Functor ((<&>))
import           Data.Tuple
import           GHC.Generics
import           System.IO
import           Types

-- TODO: Make student equality work using only the ID (implement eq instance).

instance Ord DayOfWeek where
  compare l r = compare (fromEnum l) (fromEnum r)

data Content = Content
    { _cStudents    :: [Student]
    , _cAttendances :: [Attendance]
    } deriving (Generic,Show)
$(makeLenses ''Content)

setStudents :: [Student] -> Content -> Content
setStudents = set cStudents

setAttendances :: [Attendance] -> Content -> Content
setAttendances = set cAttendances

data Connection = Connection
    { _cLocation :: String
    } deriving (Show)
$(makeLenses ''Connection)

instance ToJSON Content where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Content

instance ModelAPI Connection IO where
    createStudent :: Student -> Connection -> IO ()
    createStudent student = update' (cStudents %~ (student :))

    removeStudent studentID conn = update removeStudentImpl studentID conn
    updateStudent student conn = update updateStudentImpl student conn
    findStudent studentID conn = extractFromDatabase findStudentImpl studentID conn
    getAllStudents conn = extractStudents conn
    createAttendance attendance conn = update insertAttendanceImpl attendance conn
    removeAttendance attendanceID conn= update removeAttendanceImpl attendanceID conn
    updateAttendance attendance conn = update updateAttendanceImpl attendance conn
    findAttendance attendanceID conn =  extractFromDatabase findAttendanceImpl attendanceID conn
    getAllAttendances conn = extractAttendances conn

replaceStudent :: Student -> Student -> Student
replaceStudent st1@Student{Types._sId = id1} st2@Student{Types._sId = id2}
    | id1 == id2 = st2
    | otherwise = st1
replaceAttendance :: Attendance -> Attendance -> Attendance
replaceAttendance att1@Attendance{_aId = id1} att2@Attendance {_aId = id2}
    | id1 == id2 = att2
    | otherwise = att1
createStudentImpl    :: Student      -> Content -> Content
createStudentImpl student content@Content{_cStudents = std} = setStudents (student:std) content
removeStudentImpl    :: StudentId    -> Content -> Content
removeStudentImpl studID content@Content{_cStudents = std} = setStudents (filter (\x -> (Types._sId x) /= studID ) std) content
updateStudentImpl    :: Student      -> Content -> Content
updateStudentImpl student content@Content{_cStudents = std} = setStudents (map (\x -> replaceStudent x student ) std) content
findStudentImpl      :: StudentId    -> Content -> Maybe Student
findStudentImpl studID Content{_cStudents = std}    = find (\x -> (Types._sId x) == studID ) std
insertAttendanceImpl :: Attendance   -> Content -> Content
insertAttendanceImpl attendance content@Content{_cAttendances = att} = setAttendances (attendance : att) content
removeAttendanceImpl :: AttendanceId -> Content -> Content
removeAttendanceImpl attID content@Content{_cAttendances = att} = setAttendances (filter (\x -> (Types._aId x) /= attID ) att) content
updateAttendanceImpl :: Attendance   -> Content -> Content
updateAttendanceImpl attendance content@Content{_cAttendances = att} = setAttendances ( map (\x -> replaceAttendance x attendance) att ) content
findAttendanceImpl :: AttendanceId   -> Content -> Maybe Attendance
findAttendanceImpl attID Content{_cAttendances = att} = find (\x -> (Types._aId x) == attID ) att

update' :: (Content -> Content) -> Connection -> IO ()
update' updateF connection = do
    let storeLocation = connection ^. cLocation
    newStore <- S.readFile storeLocation
            <&> L.fromStrict
            <&> decode
            <&> maybe (Content [] [] ) updateF
            <&> encode
            <&> L.toStrict
    S.writeFile storeLocation newStore

update :: (a -> Content -> Content) -> a -> Connection -> IO ()
update function argument (Connection x) = do
    maybeContent <- decode <$>( L.fromStrict <$> (S.readFile x))
    timeUTC <- getCurrentTime
    let ioContent = (maybe (Content [] [] ) (function argument)) maybeContent
        ioEncoding = encode ioContent
        ioStrict = L.toStrict ioEncoding
    S.writeFile x ioStrict
    return ()

extractStudents :: Connection -> IO [Student]
extractStudents (Connection x) = do
    maybeIOContent <- decode <$> (L.fromStrict<$>(S.readFile x))
    return $ maybe [] (\c@Content{ _cStudents = std} -> std ) maybeIOContent

extractAttendances :: Connection -> IO [Attendance]
extractAttendances (Connection x) = do
    maybeIOContent <- decode <$> (L.fromStrict<$>(S.readFile x))
    return $ maybe [] (\c@Content{ _cAttendances = att} -> att ) maybeIOContent

extractFromDatabase function argument (Connection x) = do
    maybeIOContent <- decode <$>(L.fromStrict <$> (S.readFile x))
    return $ maybe Nothing (function argument) maybeIOContent
