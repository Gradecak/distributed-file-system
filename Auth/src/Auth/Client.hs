{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Auth.Client (disseminateToken) where

import Dir.API
import Token (Token)
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

queries :: Token -> ClientM Token
queries t = return t

disseminateToken :: Token -> [(String,Int)]-> IO ()
disseminateToken t services = do
    let destinations = genDestinations services
    manager <- newManager defaultManagerSettings
    mapM_ (runClientM (queries t) . (ClientEnv manager)) destinations

genDestinations :: [(String,Int)] -> [BaseUrl]
genDestinations x = map (\(ip,port) -> BaseUrl Http ip port "") x
