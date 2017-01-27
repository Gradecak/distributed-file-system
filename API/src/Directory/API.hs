{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Directory.API (DirAPI, dirAPI, lsEndPt, _lsEndPt,
                      openEndPt, mvEndPt, registerEndPt) where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           Utils.Data.File    (FileHandle, FileRequest)
import           Utils.InternalAuth              --for protecting internal endpts
import           Utils.Session      (TokenEndPt) --for protecting public endpts


-- | Endpoints for interacting with the directory server
-- | /authorize - add an authorized token to the token store
-- |            - @param : none
-- |            - @return : none
-- |
-- | /ls        - list the content of a given directory
-- |            - @param : "path" - path of the directory to list
-- |            - @return: list of filepaths contained in the listed dir
type DirAPI = TokenEndPt
         :<|> "list"     :> AuthProtect "cookie-auth"
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

         :<|> "register" :> ProtectInternal
                         :> ReqBody '[JSON] (String,Int)
                         :> Post '[JSON] ()

dirAPI :: Proxy DirAPI
dirAPI = Proxy

_ :<|> lsEndPt :<|> _lsEndPt :<|>openEndPt :<|> mvEndPt :<|> registerEndPt = client dirAPI
