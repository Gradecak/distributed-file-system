{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Authentication.API (AuthAPI,IPAddr,authAPI
                          , authEndPt, dirEndPt, fsysEndPt,
                            transEndPt) where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           Token
import           Utils.Data.Auth


type IPAddr = String -- a nice little alias

type AuthAPI =  "auth"  :> RemoteHost
                        :> ReqBody '[JSON] Auth
                        :> Post '[JSON] (Token, (IPAddr, Int))

           :<|> "dir"   :> RemoteHost
                        :> QueryParam "port" Int
                        :> Post '[JSON] (InternalToken, [(IPAddr, Int)])

           :<|> "fsys"  :> RemoteHost
                        :> QueryParam "port" Int
                        :> Post '[JSON] (InternalToken, [(IPAddr, Int)])

           :<|> "trans" :> RemoteHost
                        :> QueryParam "port" Int
                        :> Post '[JSON] (InternalToken, (IPAddr, Int))

authAPI :: Proxy AuthAPI
authAPI = Proxy


authEndPt :<|> dirEndPt :<|> fsysEndPt :<|> transEndPt = client authAPI
