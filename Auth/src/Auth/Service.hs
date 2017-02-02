{-# LANGUAGE OverloadedStrings #-}
module Auth.Service (runAuthService, HandlerData(..), AuthM) where

import           Auth.Internal
import           Authentication.API          (AuthAPI, Addr, authAPI)
import qualified Control.Concurrent.STM      as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.ByteString.Char8       as BS
import           Data.List.Split             (splitOn)
import qualified Database.Redis              as DB
import           Network.Socket              (SockAddr)
import           Network.Wai.Handler.Warp
import           Servant
import           Token                       (InternalToken, Token)
import           Token.Generate
import           Utils.Data.Auth (Auth(..))
import           Utils.ReaderHandler

server :: ServerT AuthAPI AuthM
server =      serveToken
         :<|> register
         :<|> newDir
         :<|> newFS
         :<|> newTrans

-- | authenticate a user with the system, generate a token and notify all
-- registered services of the new valid token returns the newly generated token
-- and touple of directory and transaction services (Dir,Trans)
serveToken :: SockAddr -> Auth -> AuthM (Token,(Addr,Addr))
serveToken ip (Auth a b) = do
    Info{redisCon=c, dirServer=d, transServer=t} <- ask
    authenticated <- liftIO $ authenticate c a b
    if authenticated
      then do
        clientToken <- generateToken (show ip)
        liftIO $ do
            ds <- TVar.readTVarIO d
            ts <- TVar.readTVarIO t
            return (clientToken, (ds,ts))
      else throwError err401 {errBody = "Authentication Failure"}


-- | Register a new user with the Distributed File System
register :: Auth -> AuthM ()
register user = do
    Info{redisCon=c} <- ask
    liftIO $ addUser user c

-- | record the new directory service
-- | Issue the new direcotry service with a session token
-- | and a list of the current file servers
newDir  :: SockAddr -> Maybe Int -> AuthM (InternalToken, [Addr])
newDir _ Nothing = throwError err417{errBody = "Missing Service Port"}
newDir addr (Just port) = do
    Info{dirServer=d, internalToken=tok, fileServers=tfs} <- ask
    liftIO $ do Stm.atomically $ TVar.writeTVar d (head $ splitOn ":" $ show addr,port)
                fs <- TVar.readTVarIO tfs
                return (tok, fs)

-- | add a new FileServer to the list of Servers and notify all other
-- | services in the system of the new service
newFS :: SockAddr -> Maybe Int-> AuthM (InternalToken, [Addr])
newFS ip Nothing     = throwError err417{errBody = "Missing Service Port"}
newFS ip (Just port) = addNewFS (head $ splitOn ":" $ show ip, port)

-- | serve the transaction server attempting to connect to the system with
-- | the internal session token and the address (Ip, port) of the directory service
newTrans :: SockAddr -> Maybe Int -> AuthM (InternalToken, Addr)
newTrans ip Nothing = throwError err417{errBody = "Missing Service Port"}
newTrans ip (Just port) = do
    Info{fileServers=f, dirServer=d, internalToken=tok, transServer=ts} <- ask
    liftIO $ do
        ds <- TVar.readTVarIO d
        Stm.atomically $ TVar.writeTVar ts (head $ splitOn ":" $ show ip,port)
        return (tok, ds)


{- Service Initialisation -}
app :: HandlerData -> Application
app inf = serve authAPI $ readerServer inf

readerServer :: HandlerData -> Server AuthAPI
readerServer inf = enter (readerToHandler inf) server

runAuthService :: IO ()
runAuthService = do
    fs <- TVar.newTVarIO []
    ds <- TVar.newTVarIO ("",-1)
    ts <- TVar.newTVarIO ("",-1)
    conn <- DB.connect DB.defaultConnectInfo
    internalTok <- genInternalToken
    run 8080 $ app (Info {fileServers=fs,
                          dirServer=ds,
                          redisCon=conn,
                          internalToken=internalTok,
                          transServer=ts})
