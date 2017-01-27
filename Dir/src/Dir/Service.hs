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
import qualified Network.Socket              as Net (SockAddr)
import           Network.Wai.Handler.Warp
import           Servant
import qualified System.Directory            as Dir
import           Token                       (InternalToken, Token)
import qualified Token.Store                 as Tok
import           Utils.Data.File             (FileHandle (..), FileRequest (..), FileMode(..))
import           Utils.FSHandler
import           Utils.Session

type FileServers = TVar.TVar [(String,Int)] -- a handy alias

-- the data that all of our handlers will have access to
data HandlerData = Info { fileServers   :: FileServers
                        , redisConn     :: Connection
                        , internalToken :: String
                        }

-- alias for the Monad our Handlers will run in
type DirM = FSHandler HandlerData

-- server
servant :: ServerT DirAPI DirM
servant =   -- public endpoint handlers
                 addAuthorized
            :<|> publicLS
            -- private Endpoint handlers
            :<|> fileSystemOp Dir.listDir
            :<|> openF
            :<|> mv
            :<|> registerFileServer

{-------------------Handlers for the Severer endpoints ----------------------------}
-- | Store the recieved 'authorized' token in the local token store
addAuthorized :: Token -> DirM NoContent
addAuthorized t = do
    Info{redisConn=c} <- ask
    liftIO $ Tok.insert t c
    return NoContent

-- | list the contents of a directory, exposed to the public
publicLS :: () -> Maybe FilePath -> DirM [FilePath]
publicLS _ Nothing  = throwError err400 {errBody="Missing file path"}
publicLS _ (Just path) = do
    x <- liftIO $ Dir.listDir path
    case x of
      Nothing        -> throwError err404 {errBody="Directory not found"}
      (Just content) -> return content

-- | move the file/directory from src to destination (Src, Dest)
mv :: Maybe String -> (FilePath, FilePath) -> DirM ()
mv tok srcDest = internalAuth tok >> (liftIO $ Dir.move $ relative srcDest)
  -- convert our touple of paths into relative paths for use with the 'shadow filesystem'
  where relative = (\(p1,p2) -> (makeRelative p1, makeRelative p2))

-- | a wrapper function for preforming filesystem opeations on the host filesystem
fileSystemOp :: (FilePath -> IO b) -> (Maybe String) -> Maybe FilePath -> DirM b
fileSystemOp op tok (Just path) = internalAuth tok >> (liftIO $ op $ makeRelative path)
fileSystemOp _  tok  Nothing = internalAuth tok >> (lift $ throwError err400 { errBody="Missing File Path"})

-- | Resolve a file request to the server the file is residing on. if the
-- requested mode is Read we will only try to read the file, returning Nothing
-- if it fails. if the requested mode is Read/ReadWrite, we will open a new file
-- if reading fails, returning the newly opened File
openF :: Maybe String -> FileRequest -> DirM (Maybe FileHandle)
openF tok (Request path Read) = internalAuth tok >> (liftIO $ getF $ makeRelative path)
openF tok (Request path _) = do
    internalAuth tok
    Info{fileServers=f} <- ask
    liftIO $ do
        file <- getF (makeRelative path) -- attempt to read file
        case file of
          Just f  -> return file         -- if file exists return file
          Nothing -> do                  -- else open new file
              fs <- TVar.readTVarIO f
              fmap Just (newFile path fs)

registerFileServer :: Maybe String -> (String, Int) -> DirM ()
registerFileServer tok ip = do
    internalAuth tok
    Info{fileServers=servers} <- ask
    void $ liftIO $ Stm.atomically $ TVar.modifyTVar' servers (ip :)
{----------------------------------------------------------------------------------}

-- | Utility function for verifying the internal session token
-- | should be called at the start of every handler that is handling a
-- | 'ProtectInternal' endpoint
internalAuth :: Maybe InternalToken -> DirM ()
internalAuth Nothing = throwError err401 {errBody="Missing service token"}
internalAuth (Just x) = do
    Info{internalToken=t} <- ask
    if x == t
       then return ()
       else throwError err401 {errBody="Missing service token"}


app :: HandlerData -> Application
app inf = serveWithContext dirAPI (genAuthServerContext $ redisConn inf) (server inf)

server :: HandlerData -> Server DirAPI
server inf = enter (readerToHandler inf) servant

-- | entry point to the Directory Service
startApp :: Int -> InternalToken -> [(String,Int)] -> IO ()
startApp port tok fileservers = do
    x    <- TVar.newTVarIO fileservers
    conn <- connect defaultConnectInfo {connectPort=(PortNumber 6380)}
    run port $ app (Info {fileServers = x, redisConn = conn, internalToken=tok})
