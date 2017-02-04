{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Exporting everything that the client will need to fully interact with the
-- system. Including members of the util library.
module Client.API (ClientState, Auth(..),
                   register, runNewClient,
                   authenticate, openFile, closeFile,
                   getFile, putFile, listDir, moveDir,
                   rmFile, runClient, FileMode(..), FileHandle(..)
                  ,File(..), FileRequest(..)) where

import           Authentication.API            (authEndPt, registerEndPt)
import           Control.Monad                 (when)
import           Control.Monad.Except
import           Control.Monad.State
import           Directory.API                 (lsEndPt)
import           File.API                      (getEndPt, putEndPt)
import           Network.HTTP.Client           (Manager, defaultManagerSettings,
                                                newManager)
import           Servant                       (ServantErr (..))
import           Servant.API.Experimental.Auth (AuthProtect)
import           Servant.Client
import           Servant.Common.Req            (Req, addHeader)
import           Token                         (Token (..))
import           Transaction.API               (closeEndPt, mvEndPt, openEndPt,
                                                rmEndPt)
import           Utils.Data.Auth               (Auth (..))
import           Utils.Data.File
import           Utils.Error

type ClientT a = StateT a (ExceptT ServantError IO)
type Address   = (String,Int)
type Username  = String
type Password  = String

data Env = Env { authAddr    :: Address
               , tranAddr    :: Address
               , dirAddr     :: Address
               , authCookie  :: String
               , credentials :: Auth
               , httpMan     :: Manager
               }

type ClientState = ClientT Env

type instance AuthClientData (AuthProtect "cookie-auth") = String

-- | to run our client we must provide client credentials and the address of
-- Authentication service, runClient will then make an authentication call to
-- the auth server.
runClient :: Auth -> Address -> StateT Env (ExceptT ServantError IO) a -> IO (Either ServantError (a, Env))
runClient auth addr a = do
    manager <- newManager defaultManagerSettings
    runExceptT (runStateT (authenticate>>a) Env{credentials=auth, authAddr=addr, httpMan=manager})

-- | running a new client will register the supplied credentials with the
-- authentication server and then authenticate.
runNewClient :: Auth -> Address -> StateT Env (ExceptT ServantError IO) a -> IO (Either ServantError (a, Env))
runNewClient auth addr a = do
    manager <- newManager defaultManagerSettings
    runExceptT (runStateT (register>>authenticate>>a) Env{credentials=auth, authAddr=addr, httpMan=manager})

-- | Add the client token header to allow for querying of protected end points
addCookieToEndPt :: (AuthenticateReq (AuthProtect "cookie-auth") -> c) -> ClientState c
addCookieToEndPt x = get >>= (\Env{authCookie=c} -> return $ x (mkAuthenticateReq c (addHeader "auth-cookie")))

{----------- State Setter functions-----------}
setAuthAddr :: Address -> ClientState ()
setAuthAddr addr = get >>= \inf -> put $ inf{authAddr=addr}

setTransAddr :: Address -> ClientState ()
setTransAddr addr = get >>= \inf -> put $ inf{tranAddr=addr}

setDirAddr :: Address -> ClientState ()
setDirAddr addr = get >>= \inf -> put $ inf{dirAddr=addr}

setToken :: String -> ClientState ()
setToken tok = get >>= \inf -> put $ inf{authCookie=tok}
{---------------------------------------------}

-- | automatically renew token if it is suspspected to be expired
tokenExpired :: ServantErr -> ClientState ()
tokenExpired err = when (errBody err == "Invalid Auth Cookie") authenticate

-- | a HOF for running servant client queries on end points
runQuery :: (Env -> Address) -> ClientM a -> ClientState a
runQuery dest endPt = do
    env@Env{httpMan=manager} <- get
    let (ip,port) = dest env
        remote = ClientEnv manager (BaseUrl Http ip port "")
    res <- liftIO $ runClientM endPt remote
    case res of
      Left err -> liftIO (print $ show err) >> throwError err
      Right x  -> liftIO (print "success" ) >> return x

register ::ClientState ()
register = get >>= \Env{credentials=c} -> runQuery authAddr (registerEndPt c)

authenticate :: ClientState ()
authenticate = do
    Env{credentials=c} <- get
    (tok, (dirAddr,transAddr)) <- runQuery authAddr (authEndPt c)
    setToken (token tok)
    setTransAddr transAddr
    setDirAddr dirAddr

openFile :: FilePath -> FileMode-> ClientState (Maybe FileHandle)
openFile path mode = do
    x <- addCookieToEndPt openEndPt
    runQuery tranAddr (x $ Request path mode)

closeFile :: FilePath -> ClientState ()
closeFile path = do
    x <- addCookieToEndPt closeEndPt
    runQuery tranAddr (x path)

getFile :: FileHandle -> ClientState (Maybe File)
getFile (FileHandle id addr) = do
    x <- addCookieToEndPt getEndPt
    runQuery (const addr) (x id)

putFile :: File -> FileHandle -> ClientState ()
putFile file (FileHandle _ addr) = do
    x <- addCookieToEndPt putEndPt
    runQuery (const addr) (x file)

listDir :: FilePath -> ClientState [FilePath]
listDir path = do
    x <- addCookieToEndPt lsEndPt
    runQuery dirAddr (x $ Just path)

moveDir :: FilePath -> FilePath -> ClientState ()
moveDir src dest = do
    x <- addCookieToEndPt mvEndPt
    runQuery dirAddr (x (src,dest))

rmFile :: FilePath -> ClientState ()
rmFile path = do
    x <- addCookieToEndPt rmEndPt
    runQuery dirAddr (x path)
    authenticate
