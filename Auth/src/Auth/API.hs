{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Auth.API (AuthAPI,IPAddr) where

import Auth
import Servant.API
import Token


type IPAddr = String -- a nice little alias

type AuthAPI =  "auth" :> RemoteHost :> ReqBody '[JSON] Auth  :> Post '[JSON] (Token, [(IPAddr, Int)])
           :<|> "dir"  :> RemoteHost :> QueryParam "port" Int :> Post '[JSON] ()
           :<|> "fsys" :> RemoteHost :> QueryParam "port" Int :> Post '[JSON] ()
