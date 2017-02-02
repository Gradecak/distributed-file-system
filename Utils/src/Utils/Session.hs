{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Utils.Session (genAuthServerContext, authHandler,
                      authenticate, _authorize) where

import           Control.Monad                    (unless)
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteString.Char8            as BS
import           Database.Redis
import           Network.Wai
import           Servant
import           Servant.Server.Experimental.Auth
import           Token
import qualified Token.Store                      as T (insert, lookupB)

type instance AuthServerData (AuthProtect "cookie-auth") = ()

genAuthServerContext :: Connection -> Context (AuthHandler Request () ': '[])
genAuthServerContext con = authHandler con  :. EmptyContext

-- | servant handler for Generalized authentication
authHandler :: Connection -> AuthHandler Request ()
authHandler c = mkAuthHandler $ handler c
  where handler con req = case lookup "auth-cookie" (requestHeaders req) of
          Nothing -> throwError (err401 { errBody = "Missing auth header" })
          Just authCookieKey -> authenticate con authCookieKey

test :: BS.ByteString -> Handler ()
test poo = if poo == "poo" then return () else  throwError (err401 {errBody= "you're not an internal service ya fucker"})

-- | lookup a token in the Redis store, if exists token is valid
authenticate :: Connection -> BS.ByteString -> Handler ()
authenticate c s = do
    x <- liftIO (T.lookupB s c)
    unless x $ throwError (err401 {errBody = "Invalid Auth Cookie"})

_authorize :: Connection -> Token -> IO ()
_authorize con tok = T.insert tok con
