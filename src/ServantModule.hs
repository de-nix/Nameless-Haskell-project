{-# LANGUAGE DataKinds, OverloadedStrings #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module ServantModule where 
import Servant.API
import Servant.Server
import Data.Time
import Types
import Data.List
import Data.Maybe
import Control.Monad.Trans
import Network.Wai.Middleware.Cors
import JSON
import Data.Proxy
import Network.Wai.Handler.Warp
import Data.Pool (Pool, withResource)
import qualified Data.Map as Map


timeFormat = "%Y-%m-%dT%H:%M:%S"
understandTime = parseTimeOrError True defaultTimeLocale timeFormat



app' :: Connection -> Application
app' conn = cors (const $ Just policy) $ serve api $ hoistServer api turnToHandler $ (getStudent conn) :<|>flip createStudent conn 
    :<|> updateStudentMethod conn:<|> flip removeStudent conn:<|> getAllStudents conn:<|>filterStudents conn :<|> getAttendance conn 
    :<|>flip createAttendance conn:<|>updateAttendanceMethod conn:<|>flip removeAttendance conn :<|> getSeminarMethod conn 
    :<|> getGroupMethod conn :<|> setStartingTimeMethod conn :<|> flip setTimetable conn
      where 
        policy = simpleCorsResourcePolicy  
          { corsRequestHeaders = ["Content-Type"]
          , corsMethods ="GET":("PUT":( "POST" : simpleMethods)) }
turnToHandler :: IO x -> Handler x
turnToHandler val = liftIO val
getStudent :: (ModelAPI a m, Monad m) => a -> StudentId -> m Student
getStudent conn sID = fromMaybe (Student (-1) "" "" "") <$> findStudent sID conn
getAttendance :: (ModelAPI a m, Monad m) => a -> AttendanceId -> m Attendance
getAttendance conn sID = fromMaybe (Attendance (-1) (-1) (-1) "" False) <$> findAttendance sID conn
getGroupMethod conn time = getGroup (understandTime time) conn
getSeminarMethod conn time  = getSeminar (understandTime time) conn
setStartingTimeMethod conn time  = setStartingTime (understandTime time) conn
updateStudentMethod conn sID body = updateStudent body conn
updateAttendanceMethod conn sID body = updateAttendance body conn
filterStudents :: (ModelAPI a m, Monad m) => a -> String ->m [Student]
filterStudents conn nameString = do 
  allStuds <- getAllStudents conn
  return $ filter (\stud -> isInfixOf nameString (name stud)) allStuds

type API = "student"  :> Capture "id" Integer :> Get '[JSON] Student
    :<|> "student"    :> ReqBody '[JSON] Student :> Post '[JSON] () 
    :<|> "student"    :> Capture "id" Integer :> ReqBody '[JSON] Student :> Put '[JSON] ()
    :<|> "student"    :> Capture "id" Integer :> Delete '[JSON] ()
    :<|> "students"   :> Get '[JSON] [Student]
    :<|> "students"   :> Capture "name" String :> Get '[JSON] [Student]
    :<|> "attendance" :> Capture "id" Integer :> Get '[JSON] Attendance
    :<|> "attendance" :> ReqBody '[JSON] Attendance :> Post '[JSON] ()
    :<|> "attendance" :> Capture "id" Integer :> ReqBody '[JSON] Attendance :> Put '[JSON] ()
    :<|> "attendance" :> Capture "id" Integer :> Delete '[JSON] ()
    :<|> "seminar"    :> Capture "time" String :> Get '[JSON] Int
    :<|> "group"      :> Capture "time" String :> Get '[JSON] String
    :<|> "time"       :> Capture "time" String :> Post '[JSON] ()
    :<|> "timetable"  :> ReqBody '[JSON] (Map.Map (DayOfWeek,Int) String) :> Post '[JSON] ()
api :: Proxy API
api = Proxy
runServer :: IO()
runServer = run 8081 $ app' $ Connection "input.in"
