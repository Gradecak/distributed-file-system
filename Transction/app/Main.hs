module Main (main) where

import Authentication.API (authAPI, transEndPt)
import Data.Proxy
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Servant.Client
import Service (startApp)
import Data.List.Split (splitOn)
import System.Environment (getEnv)
import Token (InternalToken)

registerWithAuth :: Int -> (String,Int) -> IO (Maybe (InternalToken, (String,Int)))
registerWithAuth srcPort (addr,port) = do
    manager <- newManager defaultManagerSettings
    res <- runClientM (transEndPt $ Just srcPort) (ClientEnv manager (BaseUrl Http addr port ""))
    return $ case res of
      Left err -> Nothing
      Right x -> Just x

main :: IO ()
main = do
    port         <- getEnv "TRANSACTION_PORT"
    auth_port    <- getEnv "AUTH_SERVICE_PORT"
    authResponse <- registerWithAuth (read port) ("auth-service", read auth_port)
    case authResponse of
      Nothing -> putStrLn "Cannot Establish connection to AuthServer... Aborting"
      Just (token,fileservers) -> startApp (read port) token fileservers
