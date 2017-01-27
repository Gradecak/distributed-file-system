module Main (main) where

import           Authentication.API  (authAPI, dirEndPt)
import           Data.List.Split     (splitOn)
import           Dir.Service         (startApp)
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import           Servant.Client
import           System.Environment  (getArgs)
import           Token               (InternalToken)

-- | endpoint to query
query :: Int -> ClientM (InternalToken, [(String, Int)])
query i = dirEndPt (Just i)

-- | make the query and return result
registerWithAuth :: Int -> (String,Int) -> IO (Maybe (InternalToken, [(String, Int)]))
registerWithAuth srcPort (addr,port) = do
    manager <- newManager defaultManagerSettings
    res     <- runClientM (query srcPort) (ClientEnv manager (BaseUrl Http addr port ""))
    return $ case res of
      Left err -> Nothing
      Right x  -> Just x

main :: IO ()
main = do
    port:_ <- getArgs
    putStrLn "Enter the ip and port of the auth server (<ip>:<port> format)"
    x <- getLine
    let [authIp,authPort] = splitOn ":" x
    authResponse <- registerWithAuth (read port) (authIp, read authPort)
    case authResponse of
      Nothing -> putStrLn "Cannot Establish connection to AuthServer... Aborting"
      Just (token,fileservers) -> startApp (read port) token fileservers
