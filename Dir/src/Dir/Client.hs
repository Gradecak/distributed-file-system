module Dir.Client (createFile) where

import           File.API            (createFileEndPt)
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import           Servant.Client
import           Utils.Data.File     (File)
import Token (InternalToken)


query :: a -> (a -> ClientM b) -> ClientM b
query param t =  t param

createFile :: [(String,Int)] -> File -> InternalToken -> IO ()
createFile dest file tok = do
    manager <- newManager defaultManagerSettings
    let destinations = genDestinations dest manager
    mapM_ (runClientM (createFileEndPt (Just tok) file)) destinations

genDestinations :: [(String,Int)] -> Manager -> [ClientEnv]
genDestinations x m = map (\(ip,port) -> ClientEnv m (BaseUrl Http ip port "")) x
