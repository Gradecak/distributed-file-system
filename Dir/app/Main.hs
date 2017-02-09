module Main (main) where

import           Authentication.API  (authAPI, dirEndPt)
import           Data.List.Split     (splitOn)
import           Dir.Service         (startApp)
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import           Servant.Client
import           System.Directory    (createDirectoryIfMissing)
import           System.Environment  (getEnv)
import           Token               (InternalToken)

-- | make the query and return result
registerWithAuth :: Int -> (String,Int) -> IO (Maybe (InternalToken, [(String, Int)]))
registerWithAuth srcPort (addr,port) = do
    manager <- newManager defaultManagerSettings
    res     <- runClientM (dirEndPt $ Just srcPort) (ClientEnv manager (BaseUrl Http addr port ""))
    return $ case res of
      Left err -> Nothing
      Right x  -> Just x

createShadowDir :: IO ()
createShadowDir = do
    shadow_path <- getEnv "SHADOW_PATH"
    createDirectoryIfMissing True shadow_path

main :: IO ()
main = do
    port         <- getEnv "DIRECTORY_PORT"
    auth_port    <- getEnv "AUTH_SERVICE_PORT"
    authResponse <- registerWithAuth (read port) ("auth-service", read auth_port)
    createShadowDir
    case authResponse of
      Nothing -> putStrLn "Cannot Establish connection to AuthServer... Aborting"
      Just (token,fileservers) -> startApp (read port) token fileservers
