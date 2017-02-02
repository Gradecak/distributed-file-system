module Service.Client (openFile, listDir, move, removeFile) where

import           Control.Monad       (void)
import           Directory.API       (_lsEndPt, _mvEndPt, _openEndPt, _rmEndPt)
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import           Servant.API
import           Servant.Client
import           Token               (InternalToken)
import           Utils.Data.File     (FileHandle, FileRequest)

-- | Attemp to open a file (Resolve a file to a fileserver) return
--  Nothing if error or file request is malformed
openFile :: (String,Int) -> FileRequest -> InternalToken -> IO (Maybe FileHandle)
openFile dest param tok = do
    res <- runQuery dest (_openEndPt (Just tok) param)
    return $ case res of
      Left err -> Nothing
      Right x  -> x

-- | list the contents of a filepath returns Nothing if path is
--  invalid (not a directory or doesnt exist)
listDir :: (String,Int) -> FilePath -> InternalToken -> IO (Maybe [FilePath])
listDir dest param tok = do
    res <- runQuery dest (_lsEndPt (Just tok) (Just param))
    return $ case res of
      Left err -> Nothing
      Right x  -> x

removeFile :: (String,Int) -> FilePath -> InternalToken -> IO ()
removeFile dest param tok = void $ runQuery dest (_rmEndPt (Just tok) param)

-- | Move a file/folder in the directory server. Returns nil
move :: (String,Int) -> (FilePath,FilePath) -> InternalToken -> IO ()
move dest param tok = do
    res <- runQuery dest (_mvEndPt (Just tok) param)
    case res of
      Left err -> print err
      Right x  -> return x

-- | run a query on an endpoint
runQuery :: (String,Int) -> ClientM b -> IO (Either ServantError b)
runQuery (ip,port) endpt = do
    manager <- newManager defaultManagerSettings
    runClientM (endpt) (ClientEnv manager (BaseUrl Http ip port ""))
