{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Auth.Session (genAuthServerContext, authHandler, authenticate, _addAuthorized, AuthAPI) where

import           Control.Monad                    (unless)
import qualified Data.ByteString.Char8            as BS
import           Database.Redis
import           Network.CGI                      (liftIO)
import           Network.Wai
import           Servant
import           Servant.Server.Experimental.Auth
import           Token
import qualified Token.Store                      as T (insert, lookupB)

type instance AuthServerData (AuthProtect "cookie-auth") = ()

type AuthAPI =  "authorize" :> ReqBody '[JSON] Token :> Post '[JSON] NoContent

genAuthServerContext :: Context (AuthHandler Request () ': '[])
genAuthServerContext = authHandler :. EmptyContext

-- servant handler for Generalized authentication
authHandler :: AuthHandler Request ()
authHandler = mkAuthHandler handler
  where handler req = case lookup "auth-cookie" (requestHeaders req) of
          Nothing -> throwError (err401 { errBody = "Missing auth header" })
          Just authCookieKey -> authenticate authCookieKey

-- lookup a token in the Redis store, if exists token is valid
authenticate :: BS.ByteString -> Handler()
authenticate s = do
     c <- liftIO $ connect defaultConnectInfo
     x <- liftIO (T.lookupB s c)
     unless x $ throwError (err401 {errBody = "Invalid Auth Cookie"})

-- a generic function for adding tokens to the redis token store
_addAuthorized :: Token -> Connection -> Handler NoContent
_addAuthorized t c = liftIO $ T.insert t c >> return NoContent
