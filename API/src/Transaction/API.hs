{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Transaction.API (TransAPI, transAPI) where

import Utils.Session
import Data.Proxy
import Utils.InternalAuth
import Utils.Data.File (FileRequest, FileHandle)
import Servant.API

type TransAPI = TokenEndPt

    :<|> "open" :> AuthProtect "cookie-auth"            -- /open requires authentication
                :> ReqBody '[JSON] FileRequest          -- @param FileRequest
                :> Post '[JSON] (Maybe FileHandle)      -- @return FileHandle

    :<|> "close":> AuthProtect "cookie-auth"            -- /close requires authentication
                :> ReqBody '[JSON] FilePath             -- @param FilePath
                :> Post '[JSON] ()                      -- @return Nothing

    :<|> "mv"   :> AuthProtect "cookie-auth"            -- /mv requires authentication
                :> ReqBody '[JSON] (FilePath, FilePath) -- @param (FilePath, FilePath) (src,dest)
                :> Post '[JSON] ()                      -- @return Nothing

    :<|> "rm "  :> AuthProtect "cookie-auth"            -- /rm requires authentication
                :> ReqBody '[JSON] FilePath             -- @param FilePath
                :> Post '[JSON] ()                      -- @return Nothing

transAPI :: Proxy TransAPI
transAPI = Proxy
