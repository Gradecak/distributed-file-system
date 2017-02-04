{-# LANGUAGE OverloadedStrings #-}
module FileSystem.Service (startApp) where

import qualified Control.Concurrent.STM      as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad               (unless, void, when)
import           Control.Monad.Reader        (ask, liftIO)
import           Data.Maybe                  (fromJust)
import qualified Database.MySQL.Simple       as SQL
import qualified Database.Redis              as Redis
import           File.API
import           FileSystem
import           FileSystem.Gossip
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server
import           Token                       (InternalToken, Token)
import qualified Token.Store                 as Tok
import           Utils.Data.File
import           Utils.ReaderHandler
import           Utils.Session

-- | information that our handlers will need access to in order to work
data FileServiceInfo = Info { fileServers   :: TVar.TVar [(String,Int)]
                            , redisCon      :: Redis.Connection
                            , sqlCon        :: SQL.Connection
                            , internalToken :: InternalToken
                            , gossipTable   :: TVar.TVar GossipTable
                            }

-- | Custom monad that our handlers will run in
type FileM = ReaderHandler FileServiceInfo

-- | our transformed server
servant :: ServerT FileAPI FileM
servant = -- public end points
               get
          :<|> put
          -- internal end points
          :<|> gossip
          :<|> fileOp newFile
          :<|> fileOp deleteFile
          :<|> newFileServer
          :<|> addAuthorized


newFileServer :: Maybe InternalToken -> (String,Int) -> FileM ()
newFileServer iTok addr = do
    internalAuth iTok
    Info{fileServers=fs} <- ask
    liftIO $ Stm.atomically $ TVar.modifyTVar' fs (addr:)

-- | a higher order function for creating/deleting files from our DB
fileOp :: (a -> SqlCommand) -> Maybe InternalToken -> a -> FileM ()
fileOp cmd tok fileId = do
    internalAuth tok
    Info{sqlCon=c} <- ask
    void (liftIO $ cmd fileId c)

-- | Check if the provided token is valid
internalAuth :: Maybe InternalToken -> FileM ()
internalAuth Nothing = throwError err401 {errBody="Missing service token"}
internalAuth (Just x) = do
    Info{internalToken=t} <- ask
    unless (t == x ) $ throwError err401 {errBody="Invalid Service Token"}

-- | add an authorized token to our store
addAuthorized :: Maybe InternalToken -> Token -> FileM NoContent
addAuthorized iTok newToken = do
    internalAuth iTok
    Info{redisCon=c} <- ask
    liftIO $ _authorize c newToken
    return NoContent

-- | retrieve file from database
get :: () -> String -> FileM (Maybe File)
get _ id = do
    Info{sqlCon=c} <- ask
    liftIO $ selectFile id c

-- | commit file to the database
put :: () -> File -> FileM ()
put _ file = do
    Info{sqlCon=c, fileServers=f, internalToken=tok} <- ask
    liftIO $ do
        newFile <- updateFileMeta c file -- update file version
        fs      <- TVar.readTVarIO f     -- get list of other fileservers
        updateFile newFile c             -- commit file to DB
        disseminateFile newFile fs tok   -- gossip file

-- | Update the version of the file, called on recieving a new file from client.
-- every put file operation increments the file by 1 version. if the version
-- that has been put is already ahead of the version we have, dont increment but
-- use that version instead.
updateFileMeta :: SQL.Connection -> File -> IO File
updateFileMeta conn fNew@(File _ ver id _ )= do
    fOld <- fromJust <$> selectFile id conn
    if ver > version fOld
      then return fNew
      else return fNew{version=ver+1}

-- | On recieving a gossip about a file, we will check if the file is replicated
-- in our database. If we have the file we will check the versions, if we have
-- recieved a newer version we will store the new version and gossip it forward.
-- If the file is not replicated on this fileserver, we will gossip it forward
-- but also note the fact that we have gossiped about this file in order to stop
-- infinite gossip loops.
gossip :: Maybe InternalToken -> File -> FileM ()
gossip tok file = do
    internalAuth tok
    Info{gossipTable=gossTbl, fileServers=fsList,internalToken=token} <- ask
    (table,fs) <- liftIO $ do
        t <- TVar.readTVarIO gossTbl
        f <- TVar.readTVarIO fsList
        return (t,f)
    x <- get () (fileId file) -- retrieve current version of file
    case x of
      Nothing -> liftIO $ startGossip file table fs token -- gossip
      Just x  -> compareAndPropagate (file,x) token fs    -- store and gossip

-- | only gossip the file if the version recievied is greater than the version in
-- the database. if the versions are equal the file update has already been
-- gossiped to us and we do not propaate in order to avoid propagation loops
compareAndPropagate ::(File,File) -> InternalToken -> [(String,Int)] -> FileM ()
compareAndPropagate (fNew,fOld) tok dest = when (staleFile fNew fOld) $
   put () fNew >> liftIO (disseminateFile fNew dest tok)

-- | Service Initialisation
app :: FileServiceInfo -> Application
app inf = serveWithContext fileAPI (genAuthServerContext $ redisCon inf) (server inf)

server :: FileServiceInfo -> Server FileAPI
server inf = enter (readerToHandler inf) servant

startApp :: Int -> InternalToken -> [(String,Int)] -> IO ()
startApp port tok fileserv = do
    fs       <- TVar.newTVarIO fileserv
    cache    <- newGossipCache
    goss     <- TVar.newTVarIO cache
    redisCon <- Redis.connect Redis.defaultConnectInfo {Redis.connectPort=Redis.PortNumber 6382}
    sqlCon   <- SQL.connect SQL.defaultConnectInfo{SQL.connectHost="127.0.0.1",
                                                   SQL.connectUser="root",
                                                   SQL.connectPassword="poo",
                                                   SQL.connectDatabase="files"}
    run port $ app Info{fileServers=fs, redisCon=redisCon, sqlCon=sqlCon, internalToken=tok, gossipTable=goss}
