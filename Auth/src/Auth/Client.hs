{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Auth.Client (disseminateToken, notifyNewFS) where

import           Data.Aeson
import           Data.Proxy
import           Directory.API
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import           Servant.API
import           Servant.Client
import           Utils.Session
import           Token               (Token)

_ :<|> _ :<|> _ :<|> register = client dirAPI

authorize = client tokenAPI

query :: a -> (a -> ClientM b)-> ClientM b
query a b = b a

queryAPI :: [(String,Int)] -> (a -> ClientM b) -> a -> IO ()
queryAPI endpts paramT param = do
    manager <- newManager defaultManagerSettings
    let destinations = genDestinations endpts manager

    mapM_ (runClientM (query param paramT)) destinations

disseminateToken :: Token -> [(String,Int)]-> IO ()
disseminateToken t services = queryAPI services authorize t

notifyNewFS :: (String,Int) -> [(String,Int)] -> IO ()
notifyNewFS newFS services = queryAPI services register newFS

genDestinations :: [(String,Int)] -> Manager -> [ClientEnv]
genDestinations x m = map (\(ip,port) -> ClientEnv m (BaseUrl Http ip port "")) x
