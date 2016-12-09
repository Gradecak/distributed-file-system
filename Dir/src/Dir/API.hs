{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Dir.API where

import Dir
import Auth.Session (AuthAPI)
import Servant
import Token

type DirAPI = AuthAPI
         :<|> "ls" :> AuthProtect "cookie-auth" :> QueryParam "path" FilePath :> Get '[JSON] [FilePath]
         :<|> "open"  :> AuthProtect "cookie-auth" :> ReqBody '[JSON] FileRequest :> Post '[JSON] (Maybe FileHandle)
--         :<|> "open" :> AuthProtect "cookie-auth" :> QueryParam "path" FilePath :> Get '[JSON] File
--         :<|> "close":> AuthProtect "cookie-auth" :> ReqBody '[JSON] FileHandle :> Post '[JSON] NoContent
