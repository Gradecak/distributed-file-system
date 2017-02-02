module Dir.Client (createFile,deleteFile) where

import           File.API            (createFileEndPt, deleteEndPt)
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import           Servant.Client
import           Utils.Data.File     (File(..), FileID)
import Token (InternalToken)

createFile :: [(String,Int)] -> FileID -> InternalToken -> IO ()
createFile dest file tok = runQuery dest (createFileEndPt (Just tok) file)

deleteFile :: (String,Int) -> FileID -> InternalToken -> IO ()
deleteFile dest file tok =  runQuery [dest] (deleteEndPt (Just tok) (file))

runQuery :: [(String,Int)] -> ClientM b -> IO ()
runQuery dest endpt = do
    manager <- newManager defaultManagerSettings
    let destinations = genDestinations dest manager
    mapM_ (runClientM endpt) destinations

genDestinations :: [(String,Int)] -> Manager -> [ClientEnv]
genDestinations x m = map (\(ip,port) -> ClientEnv m (BaseUrl Http ip port "")) x
