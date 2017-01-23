module Service.Client (openFile) where


import Directory.API
import Servant.Client
import Utils.Data.File
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import Servant.API

-- | Pattern match away the automatically generated Servant-Client functions
-- | for interacting with the Directory API
_ :<|> ls :<|> open :<|>_ = client dirAPI

-- | lift our request into the ClientM monad
query :: FileRequest -> ClientM (Maybe FileHandle)
query req = open req

-- | preform the query on the directory server and return response
openFile :: (String,Int) -> FileRequest -> IO (Maybe FileHandle)
openFile (ip,port) req = do
    manager <- newManager defaultManagerSettings
    res <- runClientM (query req) (ClientEnv manager (BaseUrl Http ip port ""))
    return $ case res of
      Left err -> Nothing
      Right x  -> x
