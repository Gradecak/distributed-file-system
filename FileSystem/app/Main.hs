{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Authentication.API  (authAPI)
import           Data.List.Split          (splitOn)
import           FileSystem.Service  (startApp)
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import           Servant.API
import           Servant.Client
import           System.Environment  (getArgs)
import           System.Environment  (getArgs)
import           Token               (InternalToken)

_ :<|> _ :<|> fsys :<|> _  = client authAPI

query :: Int -> ClientM (InternalToken, [(String, Int)])
query i = fsys (Just i)

registerWithAuth :: Int -> (String,Int) -> IO (Maybe (InternalToken, [(String, Int)]))
registerWithAuth srcPort (addr,port) = do
    manager <- newManager defaultManagerSettings
    res <- runClientM (query srcPort) (ClientEnv manager (BaseUrl Http addr port ""))
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
