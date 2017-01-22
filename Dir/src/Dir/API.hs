{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Dir.API where

import           Dir
import           File        (FileHandle, FileRequest)
import           Servant.API
import           Session     (AuthAPI)
import           Token


-- | Endpoints for interacting with the directory server
-- | /authorize - add an authorized token to the token store
-- |            - @param : none
-- |            - @return : none
-- |
-- | /ls        - list the content of a given directory
-- |            - @param : "path" - path of the directory to list
-- |            - @return: list of filepaths contained in the listed dirf
type DirAPI = AuthAPI
         :<|> "ls"       :> AuthProtect "cookie-auth" :> QueryParam "path" FilePath :> Get '[JSON] [FilePath]
         :<|> "open"     :> AuthProtect "cookie-auth" :> ReqBody '[JSON] FileRequest :> Post '[JSON] (Maybe FileHandle)
         :<|> "register" :> ReqBody '[JSON] (String,Int) :> Post '[JSON] ()
