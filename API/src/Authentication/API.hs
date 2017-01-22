{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Authentication.API (AuthAPI,IPAddr,authAPI) where

import Utils.Data.Auth
import Servant.API
import Data.Proxy
import Token


type IPAddr = String -- a nice little alias

type AuthAPI =  "auth" :> RemoteHost :> ReqBody '[JSON] Auth  :> Post '[JSON] (Token, (IPAddr, Int))
           :<|> "dir"  :> RemoteHost :> QueryParam "port" Int :> Post '[JSON] ()
           :<|> "fsys" :> RemoteHost :> QueryParam "port" Int :> Post '[JSON] ()

authAPI :: Proxy AuthAPI
authAPI = Proxy
