{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module ServantModule where 
import Servant.API
import Servant.Server
import Types
import Data.Maybe
import Control.Monad.Trans
import JSON
import Data.Proxy
import Network.Wai.Handler.Warp

import Data.Pool (Pool, withResource)




app' :: Connection -> Application
app' conn = serve api $ hoistServer api turnToHandler $ (getStudent conn) :<|>flip createStudent conn :<|> flip updateStudent conn:<|> flip removeStudent conn:<|> getAttendance conn :<|>flip createAttendance conn:<|>flip updateAttendance conn:<|>flip removeAttendance conn



turnToHandler :: IO x -> Handler x
turnToHandler val = liftIO val


	


getStudent :: (ModelAPI a m, Monad m) => a -> StudentId -> m Student
getStudent conn sID = fromMaybe (Student (-1) "" "" "") <$> findStudent sID conn 

getAttendance :: (ModelAPI a m, Monad m) => a -> AttendanceId -> m Attendance
getAttendance conn sID = fromMaybe (Attendance (-1) (-1) (-1) "" False) <$> findAttendance sID conn 

type API = "student" :> Capture "id" Integer :> Get '[JSON] Student
      :<|> "student" :> ReqBody '[JSON] Student :> Post '[JSON] () 
      :<|> "student" :> ReqBody '[JSON] Student :> Put '[JSON] ()
      :<|> "student" :> Capture "id" Integer :> Delete '[JSON] ()
      :<|> "attendance" :> Capture "id" Integer :> Get '[JSON] Attendance
      :<|> "attendance" :> ReqBody '[JSON] Attendance :> Post '[JSON] ()
      :<|> "attendance" :> ReqBody '[JSON] Attendance :> Put '[JSON] ()
      :<|> "attendance" :> Capture "id" Integer :> Delete '[JSON] ()


api :: Proxy API
api = Proxy

runServer = run 8081 $ app' $ Connection "input.in"
