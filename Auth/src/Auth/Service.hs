{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Auth.Service (runAuthService) where

import           Utils.Data.Auth
import           Authentication.API (AuthAPI, IPAddr, authAPI)
import           Auth.Client                 (disseminateToken, notifyNewFS)
import qualified Control.Concurrent.STM      as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Reader
import           Crypto.PasswordStore
import qualified Data.ByteString.Char8       as BS
import           Data.List.Split             (splitOn)
import qualified Database.Redis              as DB
import           Utils.FSHandler
import           Network.Socket              (SockAddr)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Token                       (Token, InternalToken)
import           Token.Generate

data ServiceInfo = Info { fileServers :: TVar.TVar [(String,Int)] -- list of file servers registered
                        , dirServer  :: TVar.TVar (String,Int)
                        , redisCon    :: DB.Connection
                        , internalToken :: String
                        }

-- essentially an alias for type AuthM = ReaderT ServiceInfo (ExceptT ServantErr IO)
type AuthM = FSHandler ServiceInfo

server :: ServerT AuthAPI AuthM
server = serveToken
         :<|> newDir
         :<|> newFS
         :<|> newTrans


-- | authenticate a user with the system, generate a token and
-- | notify all registered services of the new valid token
serveToken :: SockAddr -> Auth -> AuthM (Token, (IPAddr, Int))
serveToken ip (Auth a b) = do
    Info{redisCon=c, dirServer=d, fileServers=f} <- ask
    authenticated <- liftIO $ authenticate c a b
    if authenticated
      then liftIO $ do
        t  <- genToken (show ip)
        ds <- TVar.readTVarIO d
        fs <- TVar.readTVarIO f
        disseminateToken t (ds:fs) --notify all services of token
        return (t, ds)
      else throwError err403 {errBody = "Authentication Failure"}

-- | record the new directory service
-- | Issue the new direcotry service with a session token
-- | and a list of the current file servers
newDir  :: SockAddr -> Maybe Int -> AuthM (InternalToken, [(IPAddr, Int)])
newDir _ Nothing = throwError err417{errBody = "Missing Service Port"}
newDir addr (Just port) = do
    Info{dirServer=d, internalToken=tok, fileServers=tfs} <- ask
    fs <- liftIO $ do Stm.atomically $ TVar.writeTVar d (head $ splitOn ":" $ show addr,port)
                      TVar.readTVarIO tfs
    return (tok, fs)

-- | add a new FileServer to the list of Servers and notify all other
-- | services in the system of the new service
newFS :: SockAddr -> Maybe Int-> AuthM (InternalToken, [(IPAddr, Int)])
newFS ip Nothing = throwError err417{errBody = "Missing Service Port"}
newFS ip (Just port) = do
    Info{fileServers=f, dirServer=d, internalToken=tok} <- ask
    liftIO $ do
        fs <- TVar.readTVarIO f
        ds <- TVar.readTVarIO d
        notifyNewFS (head $ splitOn ":" $ show ip, port) (ds:fs) tok
        Stm.atomically $ TVar.modifyTVar' f  ((head $ splitOn ":" $ show ip, port) :)
        return (tok, fs)

-- | serve the transaction server attempting to connect to the system with
-- | the internal session token and the address (Ip, port) of the directory service
newTrans :: SockAddr -> Maybe Int -> AuthM (InternalToken, (IPAddr, Int))
newTrans ip Nothing = throwError err417{errBody = "Missing Service Port"}
newTrans ip (Just port) = do
    Info{fileServers=f, dirServer=d, internalToken=tok} <- ask
    liftIO $ do
        ds <- TVar.readTVarIO d
        return (tok, ds)

-- lookup the user in the database
-- @return True  -> Password == Hash
--         False -> Password
authenticate :: DB.Connection -> String -> String -> IO Bool
authenticate conn uname pswd = DB.runRedis conn $ do
    x <- DB.get (BS.pack uname)
    return $ case x of
      Right (Just y) -> verifyPassword (BS.pack pswd) y
      _              -> False

-- Service Initialisation
app :: ServiceInfo -> Application
app inf = serve authAPI $ readerServer inf

runAuthService :: IO ()
runAuthService = do
    fs <- TVar.newTVarIO []
    ds <- TVar.newTVarIO ("",-1)
    conn <- DB.connect DB.defaultConnectInfo
    internalTok <- genInternalToken
    run 8080 $ app (Info {fileServers=fs, dirServer=ds, redisCon=conn, internalToken=internalTok})

readerServer :: ServiceInfo -> Server AuthAPI
readerServer inf = enter (readerToHandler inf) server
