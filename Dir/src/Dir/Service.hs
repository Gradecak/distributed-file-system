{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Dir.Service where


import           Auth.Session
import qualified Data.ByteString.Char8            as BS
import           Database.Redis
import           Dir
import           Dir.API
import           Network.CGI                      (liftIO)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server.Experimental.Auth
import           Token
import qualified System.Directory                 as Dir

server :: Server DirAPI
server =  addAuthorized
     :<|> listFile
     :<|> getFile

addAuthorized :: Token -> Handler NoContent
addAuthorized t = liftIO (connect defaultConnectInfo) >>= _addAuthorized t

listFile :: () -> Maybe FilePath -> Handler [FilePath]
listFile _ (Just x) = liftIO $ Dir.listDirectory x
listFile _ Nothing  = throwError err400 { errBody="Missing File Path"}

getFile :: () ->  Maybe FilePath -> Handler File
getFile _ (Just x) = return $ File "asdf" "12312"
getFile _ Nothing  = throwError err400 {errBody="Missing File Path"}

-- Servant shite --
startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serveWithContext api genAuthServerContext server

api :: Proxy DirAPI
api = Proxy
