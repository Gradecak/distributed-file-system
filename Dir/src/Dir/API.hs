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
         :<|> "list" :> AuthProtect "cookie-auth" :> QueryParam "path" FilePath :> Get '[JSON] [FilePath]
         :<|> "get"  :> AuthProtect "cookie-auth" :> QueryParam "path" FilePath :> Get '[JSON] File
--         :<|> "open" :> AuthProtect "cookie-auth" :> QueryParam "path" FilePath :> Get '[JSON] File
--         :<|> "close":> AuthProtect "cookie-auth" :> QueryParam "path" FilePath :> Get '[JSON] NoContent
