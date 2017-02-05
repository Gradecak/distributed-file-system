{-# LANGUAGE OverloadedStrings #-}

module Dir.Service (startApp) where

import qualified Control.Concurrent.STM      as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Monad (unless)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Database.Redis
import           Dir
import           Directory.API               (DirAPI, dirAPI)
import qualified Network.Socket              as Net (SockAddr)
import           Network.Wai.Handler.Warp
import           Servant
import qualified System.Directory            as Dir
import           Token                       (InternalToken, Token)
import qualified Token.Store                 as Tok
import           Utils.Data.File             (FileHandle (..), FileMode (..),
                                              FileRequest (..))
import           Utils.ReaderHandler
import           Utils.Session

type FileServers = TVar.TVar [(String,Int)] -- a handy alias

-- the data that all of our handlers will have access to
data HandlerData = Info { fileServers   :: FileServers
                        , redisConn     :: Connection
                        , internalToken :: String
                        }

-- alias for the Monad our Handlers will run in
type DirM = ReaderHandler HandlerData

-- server
servant :: ServerT DirAPI DirM
servant =   -- public endpoint handlers
                 publicLS
            -- private Endpoint handlers
            :<|> ls
            :<|> openF
            :<|> mv
            :<|> rm
            :<|> registerFileServer
            :<|> addAuthorized

{-------------------Handlers for the Severer endpoints ----------------------------}
-- | Store the recieved 'authorized' token in the local token store
addAuthorized :: Maybe InternalToken -> Token -> DirM NoContent
addAuthorized tok t = do
    internalAuth tok
    liftIO $ print "got a new fucking token"
    Info{redisConn=c} <- ask
    liftIO $ Tok.insert t c
    return NoContent

-- | Remove file from the shadow file system and from the file servers on which
-- the file is replicated
rm :: Maybe InternalToken -> FilePath -> DirM ()
rm tok path = do
    Info{internalToken=t} <- ask
    internalAuth tok
    liftIO $ rmFile path t

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
mv tok srcDest = internalAuth tok >> liftIO (Dir.move $ relative srcDest)
  -- convert our touple of paths into relative paths for use with the 'shadow filesystem'
  where relative (p1,p2) = (shadow p1, shadow p2)

-- | list the contents of the provided path in the 'shadow' filesystem
ls :: Maybe String -> Maybe FilePath -> DirM (Maybe [FilePath])
ls tok (Just path) = internalAuth tok >> liftIO (listDir $ shadow path)
ls tok  Nothing    = internalAuth tok >> throwError err400 { errBody="Missing File Path"}

-- | Resolve a file request to the server the file is residing on. if the
-- requested mode is Read we will only try to read the file, returning Nothing
-- if it fails. if the requested mode is Read/ReadWrite, we will open a new file
-- if reading fails, returning the newly opened File
openF :: Maybe String -> FileRequest -> DirM (Maybe FileHandle)
openF tok (Request path Read) = internalAuth tok >> liftIO (getF path)
openF tok (Request path _) = do
    internalAuth tok
    Info{fileServers=f, internalToken=t} <- ask
    liftIO $ do
        fileHandle <- getF path        -- attempt to get fileHandle
        putStrLn "got potential file handle"
        case fileHandle of
          Just f  -> putStrLn "found file returning file" >> return fileHandle -- if file exists return fileHandle
          Nothing -> do                -- else open new file
              putStrLn $ "path" ++ show path
              fs <- TVar.readTVarIO f
              fmap Just (newFile path fs t)

registerFileServer :: Maybe String -> (String, Int) -> DirM ()
registerFileServer tok ip = do
    liftIO $ putStrLn "new file server!!"
    internalAuth tok
    liftIO $ putStrLn "new file server!!"
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
    unless (x == t) $ throwError err401 {errBody="Missing service token"}

app :: HandlerData -> Application
app inf = serveWithContext dirAPI (genAuthServerContext $ redisConn inf) (server inf)

server :: HandlerData -> Server DirAPI
server inf = enter (readerToHandler inf) servant

-- | entry point to the Directory Service
startApp :: Int -> InternalToken -> [(String,Int)] -> IO ()
startApp port tok fileservers = do
    x    <- TVar.newTVarIO fileservers
    conn <- connect defaultConnectInfo {connectHost="dir-redis"}
    run port $ app Info {fileServers = x, redisConn = conn, internalToken=tok}
