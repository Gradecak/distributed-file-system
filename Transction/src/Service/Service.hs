module Service (startApp) where

import qualified Control.Concurrent.STM      as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader
import           Data.Maybe
import qualified Database.Redis              as Redis
import           Lock
import           Servant.API
import           Servant.Server
import           Service.Client
import           Token                       (InternalToken, Token)
import           Token.Store                 (insert)
import           Transaction.API
import           Utils.Data.File
import           Utils.FSHandler
import           Utils.Session
import Network.Wai.Handler.Warp

-- | The data that our handlers will need access to
data HandlerData = Info { locks         :: TVar.TVar LockTable
                        , dirServer     :: (String,Int)
                        , internalToken :: InternalToken
                        , redisConn     :: Redis.Connection
                        }

type TransM = FSHandler HandlerData

servant :: ServerT TransAPI TransM
servant =  addAuthorized
            :<|> open
            :<|> close
            :<|> Service.move
            :<|> rm

-- | Store the recieved 'authorized' token in the local token store
addAuthorized :: Token -> TransM NoContent
addAuthorized t = do
    Info{redisConn=c} <- ask
    liftIO $ insert t c
    return NoContent

-- | Serve a users request for a file, returning a (Maybe FileHandle)
-- | if the Server cannot resolve request return Nothing
-- | if the Server cannot acquire lock return Nothing
open :: () -> FileRequest -> TransM (Maybe FileHandle)
open _ r = do
    Info{dirServer=d, internalToken=tok} <- ask
    file <- liftIO (openFile d r tok) -- resolve the request to a fileserver
    case file of
      Nothing -> return Nothing   -- if the filenname could not be resolved
      _       ->  do              -- else:
          x <- aquireFileLock r   -- lock the file
          return $ case x of
            True  -> file         -- Lock successful
            False -> Nothing      -- Lock busy

-- | Attemp to lock a file, return True if succesful False if fail
aquireFileLock :: FileRequest -> TransM Bool
aquireFileLock req@(Request path mode) = do
    Info{locks=l} <- ask
    lockMap <- liftIO $ TVar.readTVarIO l
    let locked = checkLockStatus path lockMap
    case locked of
      Locked -> return False
      Unlocked ->  do
          liftIO $ Stm.atomically $ TVar.modifyTVar' l (set req)
          return True

-- | Free the lock on a file
close :: () -> FilePath -> TransM ()
close _ path = do
    Info{locks=l} <- ask
    liftIO $ Stm.atomically $ TVar.modifyTVar' l (free path)


-- | PLACEHOLDER
rm :: () -> FilePath -> TransM ()
rm _ path = return ()

-- | Move the location of a file/directory
move :: () -> (FilePath, FilePath) -> TransM ()
move _ mv@(src, dest) = do
    Info{locks=l, dirServer=d, internalToken=tok} <- ask
    srcFs  <- liftIO $ listDir d src tok
    destFs <- liftIO $ listDir d dest tok
    let fs = (fromMaybe [] srcFs) ++ (fromMaybe [] destFs)
    lockAll fs -- will 'block' (loop) until all of the files are locked
    liftIO $ Service.Client.move d mv tok -- preform the transaction (move)
    freeAll fs -- free all of the locks aquired during move

-- | To implement transcations, we first have to make sure that the source
-- | and destination are ready to commit the transaction. To do this, we
-- | attemp to a aquire a lock on all of the files involved in the transaction
-- | In case of a failure to aquire a lock we try again until we are
-- | successful (spin lock)
lockAll :: [FilePath] -> TransM ()
lockAll = mapM_ (attemptLock)  -- will on a filepath until lock is
                                        -- aquired.
-- | free all locks on a list of files
freeAll :: [FilePath] -> TransM ()
freeAll = mapM_ (close ())

-- | Recursive spin lock function
attemptLock :: FilePath -> TransM ()
attemptLock  path = do
    status <- aquireFileLock (Request path Write) -- we need a write lock to preform move
    if status
       then return ()
       else attemptLock path

app :: HandlerData -> Application
app inf = serveWithContext transAPI (genAuthServerContext $ redisConn inf) (server inf)

server :: HandlerData -> Server TransAPI
server inf = enter (readerToHandler inf) servant

-- | entry point to the Transaction Service
startApp :: Int -> InternalToken -> (String,Int) -> IO ()
startApp port tok dirserver = do
    lockTable  <- TVar.newTVarIO (newTable) -- create an empty lock table
    conn <- Redis.connect Redis.defaultConnectInfo {Redis.connectPort=(Redis.PortNumber 6381)}
    run port $ app (Info {redisConn = conn,
                          internalToken=tok,
                          dirServer=dirserver,
                          locks=lockTable})
