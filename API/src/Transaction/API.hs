{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Transaction.API (TransAPI, transAPI, openEndPt,
                       closeEndPt, mvEndPt, rmEndPt) where

import Utils.Session
import Data.Proxy
import Utils.InternalAuth
import Utils.Data.File (FileRequest, FileHandle)
import Servant.API
import Servant.Client

type TransAPI = TokenEndPt

    :<|> "open" :> AuthProtect "cookie-auth"            -- /open (requires authentication)
                :> ReqBody '[JSON] FileRequest          -- @param FileRequest
                :> Post '[JSON] (Maybe FileHandle)      -- @return FileHandle

    :<|> "close":> AuthProtect "cookie-auth"            -- /close (requires authentication)
                :> ReqBody '[JSON] FilePath             -- @param FilePath
                :> Post '[JSON] ()                      -- @return Nothing

    :<|> "mv"   :> AuthProtect "cookie-auth"            -- /mv (requires authentication)
                :> ReqBody '[JSON] (FilePath, FilePath) -- @param (FilePath, FilePath) (src,dest)
                :> Post '[JSON] ()                      -- @return Nothing

    :<|> "rm "  :> AuthProtect "cookie-auth"            -- /rm (requires authentication)
                :> ReqBody '[JSON] FilePath             -- @param FilePath
                :> Post '[JSON] ()                      -- @return Nothing

transAPI :: Proxy TransAPI
transAPI = Proxy

_ :<|> openEndPt :<|> closeEndPt :<|> mvEndPt :<|> rmEndPt = client transAPI
