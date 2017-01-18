{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Dir.API where

import Dir
import Session (AuthAPI)
import Servant.API
import Token

type DirAPI = AuthAPI
         :<|> "ls"       :> AuthProtect "cookie-auth" :> QueryParam "path" FilePath :> Get '[JSON] [FilePath]
         :<|> "open"     :> AuthProtect "cookie-auth" :> ReqBody '[JSON] FileRequest :> Post '[JSON] (Maybe FileHandle)
         :<|> "register" :> RemoteHost                :> Post '[JSON] ()
