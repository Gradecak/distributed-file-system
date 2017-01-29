{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Transaction.API (TransAPI, transAPI, openEndPt,
                       closeEndPt, mvEndPt, rmEndPt) where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           Shared.API         (TokenEndPt)
import           Utils.Data.File    (FileHandle, FileRequest)
import           Utils.InternalAuth

type TransAPI =
         "open" :> AuthProtect "cookie-auth"            -- /open (requires authentication)
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

    :<|>        TokenEndPt

transAPI :: Proxy TransAPI
transAPI = Proxy

openEndPt :<|> closeEndPt :<|> mvEndPt :<|> rmEndPt :<|> _ = client transAPI
