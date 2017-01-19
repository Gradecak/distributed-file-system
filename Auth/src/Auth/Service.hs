{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

module Auth.Service (runAuthService) where

import           Auth
import           Auth.API
import           Auth.Client                 (disseminateToken, notifyNewFS)
import qualified Control.Concurrent.STM      as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Crypto.PasswordStore
import qualified Data.ByteString.Char8       as BS
import           Data.List.Split             (splitOn)
import qualified Database.Redis              as DB
import           FSHandler
import           Network.Socket              (SockAddr)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Token                       (Token)
import           Token.Generate

data ServiceInfo = Info { fileServers :: TVar.TVar [(String,Int)] -- list of file servers registered
                        , dirServers  :: TVar.TVar [(String,Int)]
                        , redisCon    :: DB.Connection
                        }

-- essentially an alias for type AuthM = ReaderT ServiceInfo (ExceptT ServantErr IO)
type AuthM = FSHandler ServiceInfo

server :: ServerT AuthAPI AuthM
server = serveToken
         :<|> registerService dirServers
         :<|> newFS

-- add a new FileServer to the list of Servers and notify all other
-- services in the system of the new service
newFS :: SockAddr -> Maybe Int-> AuthM ()
newFS ip Nothing = throwError err417{errBody = "Missing Service Port"}
newFS ip p@(Just port) = do
    Info{fileServers=f, dirServers=d} <- ask
    liftIO $ do
        fs <- TVar.readTVarIO f
        ds <- TVar.readTVarIO d
        notifyNewFS (head $ splitOn ":" $ show ip, port) (ds ++ fs)
    registerService fileServers ip p

-- add a service wishing to sign up to the system to the list
-- of services that should be notified on new client
registerService :: (ServiceInfo -> TVar.TVar [(String,Int)]) -> SockAddr -> Maybe Int-> AuthM ()
registerService _ _ Nothing = throwError err417{errBody = "Missing Service Port"}
registerService f ip (Just port) = do
    info <- ask
    liftIO $ Stm.atomically $ do
        TVar.modifyTVar' (f info)  ((head $ splitOn ":" $ show ip, port) :)

-- authenticate a user with the system, generate a token and
-- notify all registered services of the new valid token
serveToken :: SockAddr -> Auth -> AuthM (Token, [(IPAddr, Int)])
serveToken ip (Auth a b) = do
    Info{redisCon=c, dirServers=d, fileServers=f} <- ask
    authenticated <- liftIO $ authenticate c a b
    if authenticated
      then liftIO $ do
        t  <- genToken (show ip)
        ds <- TVar.readTVarIO d
        fs <- TVar.readTVarIO f
        disseminateToken t (ds ++ fs) --notify all services of token
        return (t, ds)
      else throwError err403 {errBody = "Authentication Failure"}

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
api :: Proxy AuthAPI
api = Proxy

app :: ServiceInfo -> Application
app inf = serve api $ readerServer inf

runAuthService :: IO ()
runAuthService = do
    fs <- TVar.newTVarIO []
    ds <- TVar.newTVarIO []
    conn <- DB.connect DB.defaultConnectInfo
    run 8080 $ app (Info {fileServers=fs, dirServers=ds, redisCon=conn})

readerServer :: ServiceInfo -> Server AuthAPI
readerServer inf = enter (readerToHandler inf) server
