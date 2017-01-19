{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Auth.Client (disseminateToken) where

import Dir.API
import Token (Token)
import Data.Aeson
import Session
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings, Manager)
import Servant.API
import Servant.Client

api :: Proxy AuthAPI
api = Proxy

authorize = client api

queries :: Token -> ClientM NoContent
queries t = authorize t

disseminateToken :: Token -> [(String,Int)]-> IO ()
disseminateToken t services = do
    manager <- newManager defaultManagerSettings
    let destinations = genDestinations services manager
    x <- mapM (runClientM (queries t)) destinations
    print x

genDestinations :: [(String,Int)] -> Manager -> [ClientEnv]
genDestinations x m = map (\(ip,port) -> ClientEnv m (BaseUrl Http ip port "")) x
