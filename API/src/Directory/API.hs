{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Directory.API (DirAPI, dirAPI) where

import           Data.Proxy
import           Servant.API
import           Token
import           Utils.Data.File (FileHandle, FileRequest)
import           Utils.Session   (TokenEndPt)


-- | Endpoints for interacting with the directory server
-- | /authorize - add an authorized token to the token store
-- |            - @param : none
-- |            - @return : none
-- |
-- | /ls        - list the content of a given directory
-- |            - @param : "path" - path of the directory to list
-- |            - @return: list of filepaths contained in the listed dirf
type DirAPI = TokenEndPt
         :<|> "ls"       :> QueryParam "path" FilePath
                         :> Get '[JSON] (Maybe [FilePath])

         :<|> "open"     :> ReqBody '[JSON] FileRequest
                         :> Post '[JSON] (Maybe FileHandle)

         :<|> "mv"       :> ReqBody '[JSON] (FilePath,FilePath)
                         :> Post '[JSON] ()

         :<|> "register" :> ReqBody '[JSON] (String,Int)
                         :> Post '[JSON] ()

dirAPI :: Proxy DirAPI
dirAPI = Proxy
