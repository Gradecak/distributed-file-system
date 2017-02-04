{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Shared.API (TokenEndPt, tokenAPI,_tokenEndPt,
                   RegisterFileServer, registerFsAPI,
                   _registerFsEndPt) where

import Servant.API
import Servant.Client
import Data.Proxy
import Token
import Utils.InternalAuth (ProtectInternal)

-- | an endpoint for recieving tokens
type TokenEndPt =  "authorize" :> ProtectInternal
                               :> ReqBody '[JSON] Token
                               :> Post '[JSON] NoContent

tokenAPI :: Proxy TokenEndPt
tokenAPI = Proxy

_tokenEndPt = client tokenAPI

type RegisterFileServer = "registerfs" :> ProtectInternal
                                        :> ReqBody '[JSON] (String,Int)
                                        :> Post '[JSON] ()

registerFsAPI :: Proxy RegisterFileServer
registerFsAPI = Proxy

_registerFsEndPt = client registerFsAPI
