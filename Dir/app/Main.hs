module Main where

import           Authentication.API  (AuthAPI, authAPI)
import           Data.List.Split     (splitOn)
import           Data.Proxy
import           Dir.Service         (startApp)
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import           Servant.API
import           Servant.Client
import           System.Environment  (getArgs)

_ :<|> dir :<|> _ = client authAPI

query :: Int -> ClientM ()
query i = dir (Just i)

registerWithAuth :: Int -> (String,Int) -> IO ()
registerWithAuth srcPort (addr,port) = do
    manager <- newManager defaultManagerSettings
    res <- runClientM (query srcPort) (ClientEnv manager (BaseUrl Http addr port ""))
    case res of
      Left err -> putStrLn $ "Error: " ++ show err
      Right x -> putStrLn $ "Server Response: " ++ show x

main :: IO ()
main = do
    port:_ <- getArgs
    putStrLn "Enter the ip and port of the auth server (<ip>:<port> format)"
    x <- getLine
    let [authIp,authPort] = splitOn ":" x
    registerWithAuth (read port) (authIp, read authPort)
    startApp (read port)
