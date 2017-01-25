module Service () where

import qualified Control.Concurrent.STM      as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader
import           Lock
import Data.Maybe
import           Service.Client
import           Transaction.API
import           Utils.Data.File
import           Utils.FSHandler

-- | The data that our handlers will need access to
data HandlerData = Info { locks     :: TVar.TVar LockTable
                        , dirServer :: (String,Int)
                        }

type TransM = FSHandler HandlerData

-- | Serve a users request for a file, returning a (Maybe FileHandle)
-- | if the Server cannot resolve request return Nothing
-- | if the Server cannot acquire lock return Nothing
open :: () -> FileRequest -> TransM (Maybe FileHandle)
open _ r = do
    Info{dirServer=d} <- ask
    file <- liftIO (openFile d r) -- resolve the request to a fileserver
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


-- | Move the location of a file/directory
move :: () -> (FilePath, FilePath) -> TransM ()
move _ mv@(src, dest) = do
    Info{locks=l, dirServer=d} <- ask
    srcFs  <- liftIO $ listDir d src
    destFs <- liftIO $ listDir d dest
    let fs = (fromMaybe [] srcFs) ++ (fromMaybe [] destFs)
    lockAll fs -- will 'block' (loop) until all of the files are locked
    liftIO $ Service.Client.move d mv -- preform the transaction (move)
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
