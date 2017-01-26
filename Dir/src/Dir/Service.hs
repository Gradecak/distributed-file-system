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
import           Token (Token,InternalToken)
import qualified Token.Store                 as Tok

type FileServers = TVar.TVar [(String,Int)] -- a handy alias

-- the data that all of our handlers will have access to
data HandlerData = Info { fileServers  :: FileServers
                        , redisConn    :: Connection
                        , internalToken :: String
                        }

-- alias for the Monad our Handlers will run in
type DirM = FSHandler HandlerData

-- server
servant :: ServerT DirAPI DirM
servant =  addAuthorized
            :<|> fileSystemOp Dir.listDir
            :<|> openF
            :<|> mv
            :<|> registerFileServer

{-------------------Handlers for the Severer endpoints ----------------------------}
internalAuth :: Maybe InternalToken -> DirM ()
internalAuth Nothing = throwError err401 {errBody="Missing service token"}
internalAuth (Just x) = do
    Info{internalToken=t} <- ask
    if x == t
       then return ()
       else throwError err401 {errBody="Missing service token"}

-- | Store the recieved 'authorized' token in the local token store
addAuthorized :: Token -> DirM NoContent
addAuthorized t = do
    Info{redisConn=c} <- ask
    liftIO $ Tok.insert t c
    return NoContent

-- | move the file/directory from src to destination (Src, Dest)
mv :: Maybe String -> (FilePath, FilePath) -> DirM ()
mv tok srcDest = internalAuth tok >> (liftIO $ Dir.move srcDest)

-- | a wrapper function for preforming filesystem opeations on the host filesystem
fileSystemOp :: (a -> IO b) -> (Maybe String) -> Maybe a -> DirM b
fileSystemOp op tok (Just x) = internalAuth tok >> (liftIO $ op x)
fileSystemOp _  tok  Nothing = internalAuth tok >> (lift $ throwError err400 { errBody="Missing File Path"})

-- | Resolve a file request to the server the file is residing on
-- | if the name can be resolved Successfully returns Just FileHandle else
-- | else returns Nothing (nil)
openF :: Maybe String -> FileRequest -> DirM (Maybe FileHandle)
openF tok (Request path mode) = internalAuth tok >> return (Just (FileHandle path "127.0.0.1"))

registerFileServer :: Maybe String -> (String, Int) -> DirM ()
registerFileServer tok ip = do
    internalAuth tok
    Info{fileServers=servers} <- ask
    void $ liftIO $ Stm.atomically $ TVar.modifyTVar' servers (ip :)
{----------------------------------------------------------------------------------}

app :: HandlerData -> Application
app inf = serveWithContext dirAPI (genAuthServerContext $ redisConn inf) (server inf)

server :: HandlerData -> Server DirAPI
server inf = enter (readerToHandler inf) servant

-- | entry point to the Directory Service
startApp :: Int -> InternalToken ->  IO ()
startApp port tok = do
    x    <- TVar.newTVarIO []
    conn <- connect defaultConnectInfo {connectPort=(PortNumber 6380)}
    run port $ app (Info {fileServers = x, redisConn = conn, internalToken=tok})
