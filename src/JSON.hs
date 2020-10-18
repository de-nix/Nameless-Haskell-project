{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module JSON where
import qualified Data.ByteString.Lazy as B 
import Types
import Data.Aeson
import GHC.Generics
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
updateStudentFromList _ [] = []

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


decodeConnection conn = do
	(Connection x) <- conn
	decode <$> (B.readFile x)


encodeConnection conn content = do
         (Connection x) <- conn	
	 B.writeFile x $ encode content

instance ModelAPI Connection where
	 


	createStudent student conn = do
                                   maybeContent <- decodeConnection conn
				   case maybeContent of 
					Nothing -> return ()
					Just (Content stds attds) ->    
				   			encodeConnection conn $ Content (student : stds ) attds 
				
      -- removeStudent  studentID conn = do
--				   (Connection x) <- conn
--                                  input <- readFile x
--
--                                   (Content stds attds) <- decode input
--                                   let newContent = Content (removeFromStudentList studentID stds ) attds in
--                                     writeFile x $ encode newContent
--                                   return ()
--
        
--	updateStudent student conn = do
--				   (Connection x) <- conn
--                                   input <- readFile x
--                                   (Content stds attds) <- decode input
 --                                  let newContent = Content (updateFromStudentList student stds ) attds in
   --                                  writeFile x $ encode newContent
    --                               return () 
--
--        findStudent  studentID conn = do
--                                   (Connection x) <- conn
--				   input <- readFile x
 --                                  (Content stds attds) <- decode input
 --                                  return $ getFromStudentList studentID stds 
--
--
 --       createAttendance attendance conn = do
--				   (Connection x) <- conn
 --                                  input <- readFile x
  --                                 (Content stds attds) <- decode input
   --                                let newContent = Content stds (attendance:attds) in
--                                     writeFile x $ encode newContent
--                                   return ()
--
--        removeAttendance attendanceID conn= do
--                                   (Connection x) <- conn
--			   	   input <- readFile x
 --                                  (Content stds attds) <- decode input
--                                   let newContent = Content stds (removeFromAttendanceList attendanceIDattds) in
--                                     writeFile x $ encode newContent
 --                                  return ()


   --     updateAttendance attendance conn= do
--				   (Connection x) <- conn
--                                   input <- readFile x
--                                   (Content stds attds) <- decode input
--                                   let newContent = Content stds (updateFromAttendanceList attendance attds ) in
--                                     writeFile x $ encode newContent
 --                                  return ()

   --     findAttendance attendanceID conn = do
  --                                 (Connection x) <- conn
--				   input <- readFile x
--                                   (Content stds attds) <- decode input
  --                                 return $ getFromAttendanceList attendanceID attds
        
