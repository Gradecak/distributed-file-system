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
--import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
--import           Servant.Server.Experimental.Auth
import           FSHandler  -- where our transformed handler monad lives
import           Session
import qualified System.Directory            as Dir
import           Token
import qualified Token.Store                 as Tok


type FileServers = TVar.TVar [String] -- a handy alias

-- the data that all of our handlers will have access to
data HandlerData = Info { fileServers :: FileServers
                        , redisConn   :: Connection }

-- alias for the Monad our Handlers will run in
type DirM = FSHandler HandlerData

--Transformed Server instance. Using our transfomed handler monad
readerServerT :: ServerT DirAPI DirM
readerServerT =  addAuthorized
            :<|> fileSystemOp Dir.listDirectory
            :<|> openF
            :<|> registerFileServer

{-------------------Handlers for the Severer endpoints ----------------------------}
addAuthorized :: Token -> DirM NoContent
addAuthorized t = do
    Info{redisConn=c} <-ask
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

-- Servant stuff
startApp :: IO ()
startApp = do
    x    <- TVar.newTVarIO []
    conn <- connect defaultConnectInfo
    run 8080 $ app (Info {fileServers = x, redisConn = conn})

app :: HandlerData -> Application
app inf = serveWithContext api (genAuthServerContext $ redisConn inf) (readerServer inf)

readerServer :: HandlerData -> Server DirAPI
readerServer inf = enter (readerToHandler inf) readerServerT

api :: Proxy DirAPI
api = Proxy
