{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module File.API (FileAPI, fileAPI) where

import Utils.Session
import Data.Proxy
import Utils.Data.File
import Utils.InternalAuth (ProtectInternal)
import Servant.API

-- | Endpoints for interacting with the fileserver
type FileAPI = TokenEndPt --endpoint to listen for new authorized clients
    -- public (client accessible) endpoints
   :<|> "get"       :> AuthProtect "cookie-auth"
                    :> ReqBody '[JSON] FilePath
                    :> Post '[JSON] (Maybe File)

   :<|> "put"       :> AuthProtect "cookie-auth"
                    :> ReqBody '[JSON] File
                    :> Post '[JSON] ()

   -- Internal End Points
   :<|> "gossip"    :> ProtectInternal
                    :> ReqBody '[JSON] (File)
                    :> Post '[JSON] ()

   :<|> "replicate" :> ProtectInternal
                    :> ReqBody '[JSON] File
                    :> Post '[JSON] ()

fileAPI :: Proxy FileAPI
fileAPI = Proxy
