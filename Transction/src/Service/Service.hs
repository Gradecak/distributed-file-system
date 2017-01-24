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
      _       ->  do
          x <- aquireFileLock r   -- lock the file
          return $ case x of
            True  -> file
            False -> Nothing

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
move _ (src, dest) = do
    Info{locks=l, dirServer=d} <- ask
    srcFs <- liftIO $ listDir d src
    destFs <- liftIO $ listDir d dest
    let fs = (fromMaybe [] srcFs) ++ (fromMaybe [] destFs)
    table <- liftIO $ TVar.readTVarIO l
    let _ = lockFiles fs table -- will block until files are locked
    liftIO $ Service.Client.move d (src, dest)


-- | Transaction is essentially a spin lock, if locking all of the src
-- | and dest files fails we will keep attempting to lock until we are succesfull
-- | only after everything is locked do we execute the tranasction
lockFiles :: [FilePath] -> LockTable -> ()
lockFiles fs table =
    let x = attemptLock fs table
    in case x of
      Locked -> lockFiles fs table
      Unlocked -> ()
