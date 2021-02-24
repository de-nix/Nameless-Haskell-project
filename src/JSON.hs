{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE InstanceSigs          #-}
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

data Connection = Connection
    { _cLocation :: String
    } deriving (Show)
$(makeLenses ''Connection)

instance ToJSON Content where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Content

instance ModelAPI Connection IO where
    createStudent :: Student   -> Connection -> IO ()
    createStudent student = update' (cStudents %~ (student :))
    
    removeStudent :: StudentId -> Connection -> IO ()
    removeStudent studentID = update' (cStudents %~ (filter ((Student studentID "" "" "" )/=)))
    
    updateStudent :: Student   -> Connection -> IO()
    updateStudent student = update' (cStudents %~ (map (\stud -> replaceStudent stud student)))

    findStudent   :: StudentId -> Connection ->IO(Maybe Student)
    findStudent studentID = queryEntity (\content ->find ((Student studentID "" "" "" )==)
                                                          (content ^. cStudents))
    
    getAllStudents :: Connection -> IO([Student])
    getAllStudents = queryList (\content -> content ^. cStudents)

    createAttendance :: Attendance -> Connection -> IO()
    createAttendance attendance = update' (cAttendances %~ (attendance: ))

    removeAttendance :: AttendanceId -> Connection -> IO()
    removeAttendance attendanceID = update' (cAttendances %~ 
      (filter ((Attendance attendanceID 0 0 "" False) /=)))

    updateAttendance :: Attendance -> Connection -> IO()
    updateAttendance attendance = update' (cAttendances %~ 
      (map (\att -> replaceAttendance att attendance)))
    
    findAttendance   :: AttendanceId -> Connection -> IO(Maybe Attendance)
    findAttendance attendanceID =  queryEntity (\content -> 
      (find((Attendance attendanceID 0 0 "" False )==) (content ^. cAttendances) ))
    
    getAllAttendances :: Connection -> IO ([Attendance])
    getAllAttendances = queryList (\content -> content ^.cAttendances)


replaceStudent :: Student -> Student -> Student
replaceStudent st1@Student{Types._sId = id1} st2@Student{Types._sId = id2}
    | id1 == id2 = st2
    | otherwise = st1
replaceAttendance :: Attendance -> Attendance -> Attendance
replaceAttendance att1@Attendance{_aId = id1} att2@Attendance {_aId = id2}
    | id1 == id2 = att2
    | otherwise = att1


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

queryEntity :: (Content-> Maybe a) -> Connection  -> IO (Maybe a)
queryEntity queryF connection = do
    let storeLocation = connection ^. cLocation
    storeData <- S.readFile storeLocation
             <&> L.fromStrict
             <&> decode
             <&> maybe Nothing queryF
    return storeData

queryList :: (Content -> [a]) -> Connection -> IO ([a])
queryList queryF connection = do
    let storeLocation = connection ^. cLocation
    storeData <- S.readFile storeLocation
             <&> L.fromStrict
             <&> decode
             <&> maybe [] queryF
    return storeData
