{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Dir.Service where

import           Auth.Session
import qualified Control.Concurrent.STM           as Stm
import qualified Control.Concurrent.STM.TVar      as TVar
import           Control.Monad.Reader
import qualified Control.Monad.Trans              as Trans (lift)
import           Control.Monad.Trans.Except
import           Database.Redis
import           Dir
import           Dir.API
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server.Experimental.Auth
import qualified System.Directory                 as Dir
import           Token
import qualified Token.Store                      as Tok
import qualified Network.Socket as Net


{-----------------Transforming Handler monad to a Directory monad ----------------}
type FileServers = TVar.TVar [String]

type DirM = ReaderT FileServers (ExceptT ServantErr IO)

readerToHandler :: FileServers -> DirM  :~> Handler
readerToHandler fs = Nat $ readerToHandler' fs

readerToHandler' :: FileServers -> forall a. DirM a -> Handler a
readerToHandler' fs r = runReaderT r fs

readerServer :: FileServers -> Server DirAPI
readerServer fs = enter (readerToHandler fs) readerServerT
{----------------------------------------------------------------------------------}

{------------------- Server instance using custom monad ---------------------------}
readerServerT :: ServerT DirAPI DirM
readerServerT =  addAuthorized
            :<|> fileOp Dir.listDirectory
            :<|> openF
            :<|> registerFileServer
{----------------------------------------------------------------------------------}

registerFileServer :: Net.SockAddr -> DirM ()
registerFileServer ip = do
    servers <- ask
    liftIO $ Stm.atomically $ TVar.modifyTVar' servers (show ip :)
    return ()

{---------------------- Handlers for the Directory Server endpoints  --------------}
addAuthorized :: Token -> DirM NoContent
addAuthorized t = do
    c <-liftIO (connect defaultConnectInfo)
    liftIO $ Tok.insert t c
    return NoContent

fileOp :: (a -> IO b) -> () -> Maybe a -> DirM b
fileOp op _ (Just x) = liftIO $ op x
fileOp _  _  Nothing = lift $ throwError err400 { errBody="Missing File Path"}

openF :: () -> FileRequest -> DirM (Maybe FileHandle)
openF _ _ = return Nothing
---------------------------------------------------------------------------------------------

startApp :: IO ()
startApp = do
    x <- TVar.newTVarIO []
    run 8080 $ app x

app :: FileServers -> Application
app fs = serveWithContext api genAuthServerContext (readerServer fs) --server

api :: Proxy DirAPI
api = Proxy
