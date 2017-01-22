{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module FileSystem.API where

import Session
import File
import Servant.API

-- | Endpoints for interacting with the fileserver
type FileAPI = AuthAPI --endpoint to listen for new authorized clients
  :<|> "get" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] FilePath :> Post '[JSON] (Maybe File)
  :<|> "put" :> AuthProtect "cookie-auth" :> ReqBody '[JSON] File :> Post '[JSON] ()
