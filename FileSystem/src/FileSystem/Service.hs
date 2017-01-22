{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module FileSystem.Service (startApp)
where

import qualified Control.Concurrent.STM      as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Reader
import qualified Database.MySQL.Simple       as SQL
import qualified Database.Redis              as Redis
import           Utils.Data.File
import           FileSystem                  (insertFile, selectFile, file)
import           FileServer.API
import           Utils.FSHandler
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server
import           Utils.Session
import           Token
import qualified Token.Store                 as Tok

-- | information that our handlers will need access to in order to work
data FileServiceInfo = Info { fileServers :: TVar.TVar [(String,Int)]
                            , redisCon    :: Redis.Connection
                            , sqlCon      :: SQL.Connection
                            }

-- | Custom monad that our handlers will run in
type FileM = FSHandler FileServiceInfo

-- | our transformed server
servant :: ServerT FileAPI FileM
servant = addAuthorized
          :<|> get
          :<|> put

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

-- | a safe head function
head' :: [a] -> Maybe a
head' [] = Nothing
head' a = Just $ head a

-- Service Initialisation

app :: FileServiceInfo -> Application
app inf = serveWithContext fileAPI (genAuthServerContext $ redisCon inf) (server inf)

server :: FileServiceInfo -> Server FileAPI
server inf = enter (readerToHandler inf) servant

startApp :: Int -> IO ()
startApp port = do
    fs <- TVar.newTVarIO []
    ds <- TVar.newTVarIO []
    redisCon <- Redis.connect Redis.defaultConnectInfo
    sqlCon <- SQL.connect SQL.defaultConnectInfo{SQL.connectHost="127.0.0.1",
                                                 SQL.connectUser="root",
                                                 SQL.connectPassword="poo",
                                                 SQL.connectDatabase="files"}
    run port $ app (Info {fileServers=fs, redisCon=redisCon, sqlCon=sqlCon})
