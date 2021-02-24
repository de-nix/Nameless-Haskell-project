{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ServantModule where
import           Data.Char
import           Control.Monad.Trans
import           Data.Function               (on)
import           Data.List
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Pool                   (Pool, withResource)
import           Data.Proxy
import           Data.Time
import           JSON
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant.API
import           Servant.Server
import           Types

app' :: Connection -> Application
app' conn = cors (const $ Just policy) $ serve api $ hoistServer api turnToHandler 
  $    flip findStudent       conn :<|> flip createStudent    conn
  :<|> flip updateStudent     conn :<|> flip removeStudent    conn
  :<|> sortedStudents         conn :<|>      filterStudents   conn
  :<|> flip findAttendance    conn :<|>      switchAttendance conn
  :<|> flip updateAttendance  conn :<|> flip removeAttendance conn
  :<|> getAttendances         conn :<|>      switchActivity   conn
      where
        policy = simpleCorsResourcePolicy
          { corsRequestHeaders = ["Content-Type"]
          , corsMethods ="DELETE" : ("GET":("PUT":( "POST" : simpleMethods))) }

turnToHandler     :: IO x -> Handler x
turnToHandler        val  =  liftIO val

filterStudents :: (ModelAPI a m, Monad m) => a -> String ->m [Student]
filterStudents conn nameString = do
  allStuds <- getAllStudents conn
  return $ filter (\stud -> isInfixOf 
                              (map toLower nameString) 
                              (map toLower ( _sName stud))
                              ) allStuds

sortedStudents :: (ModelAPI a m, Monad m) => a -> Integer -> String  -> m [Student]
sortedStudents conn seminar group = do
  allStudents    <- getAllStudents conn
  allAttendances <- getAllAttendances conn
  let filteredAttendances = filter (\Attendance{Types._aGroup = gr, _aSeminar = sem} ->
                                   gr == group && seminar >= sem
                                   ) allAttendances
      studentList = foldl (\lst student@Student{Types._sId = sid, Types._sGroup = gr} ->  
                      (student, (bonus gr group) + 
                                (getValue seminar (filter 
                                  (\Attendance{_aStudentId = asid} -> asid == sid)
                                  filteredAttendances)
                                )
                      ):lst) [] allStudents
        where
          bonus ::String -> String -> Integer
          bonus a b = case a == b of
            True -> 100
            _    -> 0
          getValue :: Integer-> [Attendance] -> Integer
          getValue seminar attendances = foldl (\nr Attendance{_aSeminar = sem} -> 
            case seminar - sem of
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
  return $ filter (\Attendance{_aSeminar = sem, Types._aGroup = gr} -> 
      (sem == seminar) && (gr == group)) allAttendances

switchAttendance :: (ModelAPI a m, Monad m) => a -> Attendance -> m()
switchAttendance conn Attendance{Types._aGroup=gr,_aSeminar=sem,_aStudentId=sid, _aActivity=act}= 
  do
    allAttendances <- getAllAttendances conn
    let maybeAtt = find (\Attendance{Types._aGroup=gr2,_aSeminar=sem2,_aStudentId = sid2} ->
                        gr2==gr && sem2==sem && sid ==sid2
                        ) allAttendances
    case maybeAtt of
      Nothing -> createAttendance (Attendance (getMaxId allAttendances) sid sem gr act) conn
      Just Attendance{_aId = aid} -> removeAttendance aid conn
    return ()
    where
      getMaxId :: [Attendance] -> Integer
      getMaxId atts = 1 + foldl (\mid Attendance{_aId = aid} -> 
                                case aid>mid of
                                  True -> aid
                                  _    -> mid
                                ) 0 atts

switchActivity   :: (ModelAPI a m, Monad m) => a -> Attendance -> m()
switchActivity conn Attendance{Types._aGroup=gr,_aSeminar=sem,_aStudentId = sid, _aActivity = act} =do
  allAttendances <- getAllAttendances conn
  let maybeAtt = find (\Attendance{Types._aGroup=gr2,_aSeminar=sem2,_aStudentId = sid2} ->
                      gr2==gr && sem2==sem && sid ==sid2) allAttendances
  case maybeAtt of
      Nothing -> createAttendance (Attendance (getMaxId allAttendances) sid sem gr act) conn
      Just Attendance{_aId = aid} -> updateAttendance (Attendance aid sid sem gr act) conn
  return ()
  where
    getMaxId :: [Attendance] -> Integer
    getMaxId atts = 1 + foldl (\mid Attendance{_aId = aid} -> 
                              case aid>mid of
                                True -> aid
                                _    -> mid
                              ) 0 atts

type API = "student"         :> Capture "id" Integer :> Get '[JSON] (Maybe Student)
    :<|> "student"           :> ReqBody '[JSON] Student :> Post '[JSON] ()
    :<|> "student"           :> ReqBody '[JSON] Student :> Put '[JSON] ()
    :<|> "student"           :> Capture "id" Integer :> Delete '[JSON] ()
    :<|> "students":>"sort"  :> Capture "seminar" Integer :> Capture "group" String :> Get '[JSON] [Student]
    :<|> "students":>"filter":> Capture "name" String :> Get '[JSON] [Student]
    :<|> "attendance"        :> Capture "id" Integer :> Get '[JSON] (Maybe Attendance)
    :<|> "attendance"        :> ReqBody '[JSON] Attendance :> Post '[JSON] ()
    :<|> "attendance"        :> ReqBody '[JSON] Attendance :> Put '[JSON] ()
    :<|> "attendance"        :> Capture "id" Integer :> Delete '[JSON] ()
    :<|> "attendances"       :> Capture "seminar" Integer :> Capture "group" String :> Get '[JSON] [Attendance]
    :<|> "activity"          :> ReqBody '[JSON] Attendance :> Post '[JSON] ()
api :: Proxy API
api = Proxy
runServer :: IO()
runServer = run 8081 $ app' $ Connection "input.in"
