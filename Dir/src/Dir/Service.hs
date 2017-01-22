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
import           Directory.API
import           Utils.Data.File (FileHandle(..), FileRequest(..))
import           Utils.FSHandler
import qualified Network.Socket              as Net (SockAddr)
import           Network.Wai.Handler.Warp
import           Servant
import           Utils.Session
import qualified System.Directory            as Dir
import           Token
import qualified Token.Store                 as Tok

type FileServers = TVar.TVar [(String,Int)] -- a handy alias

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

-- | Store the recieved 'authorized' token in the local token store
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
openF _ (Request path mode) = return (Just (FileHandle path "127.0.0.1"))

registerFileServer :: (String, Int) -> DirM ()
registerFileServer ip = do
    Info{fileServers=servers} <- ask
    void $ liftIO $ Stm.atomically $ TVar.modifyTVar' servers (ip :)
{----------------------------------------------------------------------------------}

app :: HandlerData -> Application
app inf = serveWithContext dirAPI (genAuthServerContext $ redisConn inf) (server inf)

server :: HandlerData -> Server DirAPI
server inf = enter (readerToHandler inf) servant

-- entry point to the Directory Service
startApp :: Int -> IO ()
startApp port = do
    x    <- TVar.newTVarIO []
    conn <- connect defaultConnectInfo {connectPort=(PortNumber 6380)}
    run port $ app (Info {fileServers = x, redisConn = conn})
