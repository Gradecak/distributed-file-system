{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Auth.Client (disseminateToken, notifyNewFS) where

import           Directory.API
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import           Servant.API
import           Servant.Client
import           Token               (Token, InternalToken)
import           Utils.Session


-- | Pattern match away the automatically generated Servant-Client functions
-- | for interacting with the Directory API
_ :<|> _ :<|> _ :<|> _ :<|> register = client dirAPI
authorize                            = client tokenAPI

-- | a HOF for lifting a query into the ClientM monad
query :: a -> (a -> ClientM b)-> ClientM b
query a b = b a

test :: a -> b -> (a -> b -> ClientM c) -> ClientM c
test a b f = f a b

-- | a HOF for querying a set of endpoints
-- | given the Endpoints.                   -> [(String,Int)]
-- | the function for interacting with api. -> (a -> ClientM b)
-- | and the data to send to the end point. -> a
queryAPI :: [(String,Int)] -> (a -> ClientM b) -> a -> IO ()
queryAPI endpts paramT param = do
    manager <- newManager defaultManagerSettings
    let destinations = genDestinations endpts manager
    mapM_ (runClientM (query param paramT)) destinations

-- | broadcast a token to all of the registered services
disseminateToken :: Token -> [(String,Int)]-> IO ()
disseminateToken t services = queryAPI services authorize t

-- | notify all of the registered services of a new file server
notifyNewFS :: (String,Int) -> [(String,Int)] -> InternalToken -> IO ()
notifyNewFS newFS services token = queryAPI services (register $ Just token) newFS

-- | Convert the destingations from [(String,Int)] format to a
-- | Servant-Client usable format [ClientEnv]
genDestinations :: [(String,Int)] -> Manager -> [ClientEnv]
genDestinations x m = map (\(ip,port) -> ClientEnv m (BaseUrl Http ip port "")) x
