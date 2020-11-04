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

type API = "student" :> Capture "id" Int :> Get '[JSON] Student
       -- :<|> "attendance" :> Capture "id" Int :> Get '[JSON] Attendance
       -- :<|> "student" :> ReqBody '[JSON] Student :> Post '[JSON] () 

server :: Server API
server = getStudent

getStudent :: Int -> Handler Student
getStudent x = liftIO$ fromMaybe (Student (-1) "" "" "")<$> findStudent (toInteger x) (Connection "input.in") 

proxyAPI :: Proxy API
proxyAPI = Proxy
app :: Application
app = serve proxyAPI server
runServer = run 8081 app
