module Service.Client (openFile, listDir, move) where


import Directory.API
import Servant.Client
import Utils.Data.File
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import Servant.API

-- | Pattern match away the automatically generated Servant-Client functions
-- | for interacting with the Directory API
_ :<|> ls :<|> open :<|> mv :<|> _ = client dirAPI

-- | lift our request into the ClientM monad
query :: a -> (a -> ClientM b) -> ClientM b
query a b = b a

-- | attemp to open a file (Resolve a file to a fileserver)
-- | return Nothing if error or file request is malformed
openFile :: (String,Int) -> FileRequest -> IO (Maybe FileHandle)
openFile dest param = do
    res <- runQuery dest param open
    return $ case res of
      Left err -> Nothing
      Right x  -> x

-- | list the contents of a filepath
-- | returns Nothing if path is invalid (not a directory or doesnt exist)
listDir :: (String,Int) -> FilePath -> IO (Maybe [FilePath])
listDir dest param = do
    res <- runQuery dest (Just param) ls
    return $ case res of
      Left err -> Nothing
      Right x  -> x

-- | Move a file/folder in the directory server
-- | returns nothing
move :: (String,Int) -> (FilePath,FilePath) -> IO ()
move dest param = do
    res <- runQuery dest param mv
    case res of
      Left err -> print err
      Right x -> return x

-- | run a query on an endpoint
runQuery :: (String,Int) -> a -> (a -> ClientM b) -> IO (Either ServantError b)
runQuery (ip,port) param endpt = do
    manager <- newManager defaultManagerSettings
    runClientM (query param endpt)  (ClientEnv manager (BaseUrl Http ip port ""))
