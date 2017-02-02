{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Authentication.API (AuthAPI,Addr,authAPI
                          , authEndPt, dirEndPt, fsysEndPt,
                            transEndPt, registerEndPt) where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           Token
import           Utils.Data.Auth

type Addr = (String,Int) -- a nice little alias

type AuthAPI =  "auth"     :> RemoteHost
                           :> ReqBody '[JSON] Auth
                           :> Post '[JSON] (Token, (Addr,Addr))

           :<|> "register" :> ReqBody '[JSON] Auth
                           :> Post '[JSON] ()

           :<|> "dir"      :> RemoteHost
                           :> QueryParam "port" Int
                           :> Post '[JSON] (InternalToken, [Addr])

           :<|> "fsys"     :> RemoteHost
                           :> QueryParam "port" Int
                           :> Post '[JSON] (InternalToken, [Addr])

           :<|> "trans"    :> RemoteHost
                           :> QueryParam "port" Int
                           :> Post '[JSON] (InternalToken, Addr)

authAPI :: Proxy AuthAPI
authAPI = Proxy

authEndPt :<|> registerEndPt :<|>dirEndPt :<|> fsysEndPt :<|> transEndPt = client authAPI
