{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Dir.Service where

import           Auth.Session
import           Control.Applicative              ((<$>))
import           Database.Redis
import           Dir
import           Dir.API
import           Network.CGI                      (liftIO)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server.Experimental.Auth
import qualified System.Directory                 as Dir
import           Token
import qualified Token.Store as Tok
import Control.Monad.Reader
import qualified Control.Monad.Trans as Trans (lift)

-- server instance using the servant Handler monad
server :: Server DirAPI
server =  addAuthorized
     :<|> fileOp Dir.listDirectory
     :<|> openF

-- server instance using the Reader monad (transformed from Handler monad)
readerServerT :: ServerT DirAPI (Reader [String])
readerServerT =  addAuthorized1
                 :<|> fileOp Dir.listDirectory
                 :<|> openF

-- handler for adding authorize tokens to local redis instance
-- (using Reader monad)
addAuthorized1 :: Token -> Reader [String] NoContent
addAuthorized1 t = liftIO (connect defaultConnectInfo) >>= Tok.insert t >> return NoContent

-- handler for adding authorize otkens to local redis instance
-- (using Handler monad)
addAuthorized :: Token -> Handler NoContent
addAuthorized t = liftIO (connect defaultConnectInfo) >>= _addAuthorized t

-- placeholder for handling open file requests
openF :: () -> FileRequest -> Handler(Maybe FileHandle)
openF _ _ = return Nothing

-- functions for transfomring Handler to Reader monad--------------------
readerToHandler :: Reader [String] :~> Handler               --
readerToHandler = Nat readerToHandler'                       --
                                                             --
readerToHandler' :: forall a. Reader [String] a -> Handler a --
readerToHandler' r = return (runReader r [])                 --

readerServer :: Server DirAPI                                --
readerServer = enter readerToHandler readerServerT           --
-------------------------------------------------------------------------


-- HOF for preforming file system operations and
-- lifting result into the Handler monad
fileOp :: (a -> IO b) -> () -> Maybe a -> Handler b
fileOp op _ (Just x) = liftIO $ op x
fileOp _  _  Nothing = throwError err400 { errBody="Missing File Path"}

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serveWithContext api genAuthServerContext readerServer  --server

api :: Proxy DirAPI
api = Proxy
