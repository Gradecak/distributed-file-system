{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Dir.Service where

import qualified Control.Concurrent.STM      as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Database.Redis
import           Dir
import           Dir.API
import qualified Network.Socket              as Net
import           Network.Wai.Handler.Warp
import           Servant
import           FSHandler  -- where our transformed handler monad lives
import           Session
import qualified System.Directory            as Dir
import           Token
import qualified Token.Store                 as Tok

type FileServers = TVar.TVar [String] -- a handy alias

-- the data that all of our handlers will have access to
data HandlerData = Info { fileServers :: FileServers
                        , redisConn   :: Connection
                        }

-- alias for the Monad our Handlers will run in
type DirM = FSHandler HandlerData

-- server
servant :: ServerT DirAPI DirM
servant =  addAuthorized
            :<|> fileSystemOp Dir.listDirectory
            :<|> openF
            :<|> registerFileServer

{-------------------Handlers for the Severer endpoints ----------------------------}
addAuthorized :: Token -> DirM NoContent
addAuthorized t = do
    Info{redisConn=c} <- ask
    liftIO $ Tok.insert t c
    return NoContent

-- perform an action on the host filesystem
fileSystemOp :: (a -> IO b) -> () -> Maybe a -> DirM b
fileSystemOp op _ (Just x) = liftIO $ op x
fileSystemOp _  _  Nothing = lift $ throwError err400 { errBody="Missing File Path"}

openF :: () -> FileRequest -> DirM (Maybe FileHandle)
openF _ (Request path mode) = return Nothing

registerFileServer :: Net.SockAddr -> DirM ()
registerFileServer ip = do
    Info{fileServers=servers} <- ask
    void $ liftIO $ Stm.atomically $ TVar.modifyTVar' servers (show ip :)
{----------------------------------------------------------------------------------}


app :: HandlerData -> Application
app inf = serveWithContext api (genAuthServerContext $ redisConn inf) (server inf)

server :: HandlerData -> Server DirAPI
server inf = enter (readerToHandler inf) servant

api :: Proxy DirAPI
api = Proxy

-- entry point to the Directory Service
startApp :: IO ()
startApp = do
    x    <- TVar.newTVarIO []
    conn <- connect defaultConnectInfo {connectPort=(PortNumber 6380)}
    run 8081 $ app (Info {fileServers = x, redisConn = conn})
