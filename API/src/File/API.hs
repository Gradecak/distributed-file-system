{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module File.API (FileAPI, fileAPI, getEndPt, putEndPt,
                gossipEndPt, createFileEndPt) where

import Utils.Session
import Data.Proxy
import Utils.Data.File
import Utils.InternalAuth (ProtectInternal)
import Servant.API
import Servant.Client

-- | Endpoints for interacting with the fileserver
type FileAPI = TokenEndPt --endpoint to listen for new authorized clients
    -- public (client accessible) endpoints
   :<|> "get"    :> AuthProtect "cookie-auth"
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
                 :> ReqBody '[JSON] File
                 :> Post '[JSON] ()

fileAPI :: Proxy FileAPI
fileAPI = Proxy

_ :<|> getEndPt :<|> putEndPt :<|> gossipEndPt :<|> createFileEndPt = client fileAPI
