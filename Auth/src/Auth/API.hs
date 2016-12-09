{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Auth.API (AuthAPI) where

import Auth
import Servant.API
import Token

type AuthAPI = "auth" :> RemoteHost :> ReqBody '[JSON] Auth :> Post '[JSON] (Token, String)
