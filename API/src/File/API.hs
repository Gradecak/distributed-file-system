{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
module File.API (FileAPI, fileAPI, getEndPt, putEndPt,
                gossipEndPt, createFileEndPt, deleteEndPt) where

import Shared.API (TokenEndPt, RegisterFileServer)
import Data.Proxy
import Utils.Data.File
import Utils.InternalAuth (ProtectInternal)
import Servant.API
import Servant.Client

-- | Endpoints for interacting with the fileserver
type FileAPI =
    -- public (client accessible) endpoints
        "get"    :> AuthProtect "cookie-auth"
                 :> ReqBody '[JSON] FilePath
                 :> Post '[JSON] (Maybe File)

   :<|> "put"    :> AuthProtect "cookie-auth"
                 :> ReqBody '[JSON] File
                 :> Post '[JSON] ()

   -- Internal End Points
   :<|> "gossip" :> ProtectInternal
                 :> ReqBody '[JSON] (File)
                 :> Post '[JSON] ()

   :<|> "create" :> ProtectInternal
                 :> ReqBody '[JSON] FileID
                 :> Post '[JSON] ()

   :<|> "delete" :> ProtectInternal
                 :> ReqBody '[JSON] FileID
                 :> Post '[JSON] ()

   :<|> TokenEndPt

   :<|> RegisterFileServer

fileAPI :: Proxy FileAPI
fileAPI = Proxy

getEndPt :<|> putEndPt :<|> gossipEndPt :<|> createFileEndPt :<|> deleteEndPt :<|> _ :<|> _ = client fileAPI
