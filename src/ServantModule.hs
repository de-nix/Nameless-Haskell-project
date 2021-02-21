{-# LANGUAGE DataKinds, OverloadedStrings #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module ServantModule where 
import Servant.API
import Servant.Server
import Data.Time
import Types
import Data.Function (on)
import Data.List
import Data.Maybe
import Control.Monad.Trans
import Network.Wai.Middleware.Cors
import JSON
import Data.Proxy
import Network.Wai.Handler.Warp
import Data.Pool (Pool, withResource)
import qualified Data.Map as Map

app' :: Connection -> Application
app' conn = cors (const $ Just policy) $ serve api $ hoistServer api turnToHandler $ 
  getStudent                  conn :<|> flip createStudent    conn 
  :<|> updateStudentMethod    conn :<|> flip removeStudent    conn
  :<|> sortedStudents         conn :<|>      filterStudents   conn 
  :<|> getAttendance          conn :<|>      switchAttendance conn
  :<|> updateAttendanceMethod conn :<|> flip removeAttendance conn 
  :<|> getAttendances         conn :<|>      switchActivity   conn
      where 
        policy = simpleCorsResourcePolicy  
          { corsRequestHeaders = ["Content-Type"]
          , corsMethods ="GET":("PUT":( "POST" : simpleMethods)) }

turnToHandler     :: IO x -> Handler x
turnToHandler        val  =  liftIO val

getStudent        :: (ModelAPI a m, Monad m) => a -> StudentId -> m Student
getStudent    conn sID = fromMaybe (Student (-1) "" "" "") <$> findStudent sID conn
getAttendance     :: (ModelAPI a m, Monad m) => a -> AttendanceId -> m Attendance
getAttendance conn sID = fromMaybe (Attendance (-1) (-1) (-1) "" False) <$> findAttendance sID conn

updateStudentMethod conn sID body = updateStudent body conn
updateAttendanceMethod conn sID body = updateAttendance body conn

filterStudents :: (ModelAPI a m, Monad m) => a -> String ->m [Student]
filterStudents conn nameString = do 
  allStuds <- getAllStudents conn
  return $ filter (\stud -> isInfixOf nameString (name stud)) allStuds

sortedStudents :: (ModelAPI a m, Monad m) => a -> Integer -> String  -> m[Student]
sortedStudents conn seminar group = do
  allStudents <- getAllStudents conn
  allAttendances <- getAllAttendances conn
  let filteredAttendances = filter (\Attendance{Types.group = gr, seminar = sem} ->gr == group && seminar >= sem) allAttendances
      studentList = foldl (\lst student@Student{Types.id = sid, Types.studentGroup = gr} ->  (student, (bonus gr group) + (getValue seminar (filter (\Attendance{studentId = asid} -> asid == sid) filteredAttendances))):lst) [] allStudents
        where 
          bonus ::String -> String -> Integer
          bonus a b = case a == b of
            True -> 100
            _    -> 0
          getValue :: Integer-> [Attendance] -> Integer
          getValue seminar attendances = foldl (\nr Attendance{seminar = sem} -> case seminar - sem of
            0 -> nr + 500
            1 -> nr + 100
            2 -> nr + 50
            3 -> nr + 5
            _ -> nr + 1
            ) 0 attendances
      sortedList = take 40 $ sortBy (flip (compare `on` snd)) studentList
  return $ map fst sortedList


getAttendances ::(ModelAPI a m, Monad m) => a -> Integer -> String -> m [Attendance]
getAttendances conn seminar group = do
  allAttendances <- getAllAttendances conn
  return $ filter (\Attendance{seminar = sem, Types.group = gr} -> (sem == seminar) && (gr == group)) allAttendances


switchAttendance :: (ModelAPI a m, Monad m) => a -> Attendance -> m()
switchAttendance conn Attendance{Types.group=gr,seminar=sem,studentId = sid, activity = act} = do
  allAttendances <- getAllAttendances conn
  let maybeAtt = find (\Attendance{Types.group=gr2,seminar=sem2,studentId = sid2} -> gr2==gr && sem2==sem && sid ==sid2) allAttendances
  case maybeAtt of
    Nothing -> createAttendance (Attendance (getMaxId allAttendances) sid sem gr act) conn
    Just Attendance{attendanceId = aid} -> removeAttendance aid conn
  return ()
  where
    getMaxId :: [Attendance] -> Integer
    getMaxId atts = 1 + foldl (\mid Attendance{attendanceId = aid} -> case aid>mid of 
                                                                      True -> aid
                                                                      _    -> mid) 0 atts
    
switchActivity   :: (ModelAPI a m, Monad m) => a -> Attendance -> m()
switchActivity conn Attendance{Types.group=gr,seminar=sem,studentId = sid, activity = act} =do 
  allAttendances <- getAllAttendances conn
  let maybeAtt = find (\Attendance{Types.group=gr2,seminar=sem2,studentId = sid2} -> gr2==gr && sem2==sem && sid ==sid2) allAttendances
  case maybeAtt of
      Nothing -> createAttendance (Attendance (getMaxId allAttendances) sid sem gr act) conn
      Just Attendance{attendanceId = aid} -> updateAttendance (Attendance aid sid sem gr act) conn
  return ()
  where
    getMaxId :: [Attendance] -> Integer
    getMaxId atts = 1 + foldl (\mid Attendance{attendanceId = aid} -> case aid>mid of
                                                                           True -> aid
                                                                           _    -> mid) 0 atts

type API = "student"         :> Capture "id" Integer :> Get '[JSON] Student
    :<|> "student"           :> ReqBody '[JSON] Student :> Post '[JSON] () 
    :<|> "student"           :> Capture "id" Integer :> ReqBody '[JSON] Student :> Put '[JSON] ()
    :<|> "student"           :> Capture "id" Integer :> Delete '[JSON] ()
    :<|> "students" :>"sort" :> Capture "seminar" Integer :> Capture "group" String :> Get '[JSON] [Student]
    :<|> "students" :>"filter":> Capture "name" String :> Get '[JSON] [Student]
    :<|> "attendance"        :> Capture "id" Integer :> Get '[JSON] Attendance
    :<|> "attendance"        :> ReqBody '[JSON] Attendance :> Post '[JSON] ()
    :<|> "attendance"        :> Capture "id" Integer :> ReqBody '[JSON] Attendance :> Put '[JSON] ()
    :<|> "attendance"        :> Capture "id" Integer :> Delete '[JSON] ()
    :<|> "attendances"       :> Capture "seminar" Integer :> Capture "group" String :> Get '[JSON] [Attendance]
    :<|> "activity"          :> ReqBody '[JSON] Attendance :> Post '[JSON] ()
api :: Proxy API
api = Proxy
runServer :: IO()
runServer = run 8081 $ app' $ Connection "input.in"
