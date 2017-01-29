{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Directory.API (DirAPI, dirAPI, lsEndPt, _lsEndPt,
                      _openEndPt, _mvEndPt, _registerEndPt,
                      _rmEndPt) where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           Shared.API
import           Utils.Data.File    (FileHandle, FileRequest)
import           Utils.InternalAuth


-- | Endpoints for interacting with the directory server
-- | /authorize - add an authorized token to the token store
-- |            - @param : none
-- |            - @return : none
-- |
-- | /ls        - list the content of a given directory
-- |            - @param : "path" - path of the directory to list
-- |            - @return: list of filepaths contained in the listed dir
type DirAPI =
              "list"     :> AuthProtect "cookie-auth"
                         :> QueryParam "path" FilePath
                         :> Get '[JSON] ([FilePath])

         :<|> "ls"       :> ProtectInternal
                         :> QueryParam "path" FilePath
                         :> Get '[JSON] (Maybe [FilePath])

         :<|> "open"     :> ProtectInternal
                         :> ReqBody '[JSON] FileRequest
                         :> Post '[JSON] (Maybe FileHandle)

         :<|> "mv"       :> ProtectInternal
                         :> ReqBody '[JSON] (FilePath,FilePath)
                         :> Post '[JSON] ()

         :<|> "rm"       :> ProtectInternal
                         :> ReqBody '[JSON] FilePath
                         :> Post '[JSON] ()

         :<|>            RegisterFileServer

         :<|>            TokenEndPt

dirAPI :: Proxy DirAPI
dirAPI = Proxy

lsEndPt :<|> _lsEndPt :<|> _openEndPt :<|> _mvEndPt :<|> _rmEndPt:<|> _registerEndPt :<|> _ = client dirAPI
