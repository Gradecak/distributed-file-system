{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module FileSystem.Service (startApp) where

import qualified Control.Concurrent.STM      as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Reader
import qualified Database.MySQL.Simple       as SQL
import qualified Database.Redis              as Redis
import           File.API
import           FileSystem                  (file, insertFile, selectFile)
import           FileSystem.Gossip
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server
import           Token                       (InternalToken, Token)
import qualified Token.Store                 as Tok
import           Utils.Data.File
import           Utils.FSHandler
import           Utils.Session

-- | information that our handlers will need access to in order to work
data FileServiceInfo = Info { fileServers   :: TVar.TVar [(String,Int)]
                            , redisCon      :: Redis.Connection
                            , sqlCon        :: SQL.Connection
                            , internalToken :: InternalToken
                            , gossipTable   :: TVar.TVar GossipTable
                            }

-- | Custom monad that our handlers will run in
type FileM = FSHandler FileServiceInfo

-- | our transformed server
servant :: ServerT FileAPI FileM
servant = addAuthorized
          :<|> get
          :<|> put
          :<|> gossip
          :<|> create

create :: Maybe InternalToken -> File -> FileM ()
create tok file = internalAuth tok >> put () file

internalAuth :: Maybe InternalToken -> FileM ()
internalAuth Nothing = throwError err401 {errBody="Missing service token"}
internalAuth (Just x) = do
    Info{internalToken=t} <- ask
    if x == t
       then return ()
       else throwError err401 {errBody="Missing service token"}

-- | add an authorized token to our store
addAuthorized :: Token -> FileM NoContent
addAuthorized t = do
    Info{redisCon=c} <- ask
    liftIO $ _authorize c t
    return NoContent

-- | retrieve file from database
get :: () -> FilePath -> FileM (Maybe File)
get _ path = do
    Info{sqlCon=c} <- ask
    x <- liftIO $ selectFile [path] c
    return $ head' x

-- | commit file to the database
put :: () -> File -> FileM ()
put _ file = do
    Info{sqlCon=c} <- ask
    liftIO $ insertFile file c
    return ()

gossip :: Maybe InternalToken -> File -> FileM ()
gossip tok file = do
    Info{gossipTable=gossTbl, fileServers=fsList, internalToken=token} <- ask
    internalAuth tok
    (table,fs) <- liftIO $ do
        t      <- TVar.readTVarIO gossTbl
        f      <- TVar.readTVarIO fsList
        return (t,f)
    x <- get () (filepath file) -- retrieve current version of file
    case x of
      Nothing -> liftIO $ startGossip file table fs token
      Just x  -> compareVersions (file,x) token fs

compareVersions ::(File,File) -> InternalToken -> [(String,Int)] -> FileM ()
compareVersions (fNew,fOld) tok dest = do
    if staleFile fNew fOld
       then put () fNew >> (liftIO $ disseminateFile fNew dest tok)
       else return ()

-- | a safe head function
-- | used for safely extracting the result of a database query
head' :: [a] -> Maybe a
head' [] = Nothing
head' a  = Just $ head a

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
    redisCon <- Redis.connect Redis.defaultConnectInfo {Redis.connectPort=(Redis.PortNumber 6382)}
    sqlCon   <- SQL.connect SQL.defaultConnectInfo{SQL.connectHost="127.0.0.1",
                                                   SQL.connectUser="root",
                                                   SQL.connectPassword="poo",
                                                   SQL.connectDatabase="files"}
    run port $ app (Info {fileServers=fs, redisCon=redisCon, sqlCon=sqlCon, internalToken=tok, gossipTable=goss})
