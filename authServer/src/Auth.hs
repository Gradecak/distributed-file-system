{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Auth where

import           Crypto.PasswordStore
import           Data.Aeson
import qualified Data.ByteString.Char8              as BS (ByteString, pack)
import           GHC.Generics
import           Network.CGI                        (liftIO)
import           Network.Wai
import           Database.Redis
import           Servant
import           Token                              (Token)
import           Token.Generate                     (genToken)

data User = User { name :: String, hash :: String } deriving (Show)

data AuthReq = Auth { username :: String
                    , password :: String
                    } deriving (Eq, Show, Generic, FromJSON)

-- auth server enpoints
type AuthAPI = "auth" :> ReqBody '[JSON] AuthReq :> Post '[JSON] Token

server :: Server AuthAPI
server = serveToken

serveToken :: AuthReq -> Handler Token
serveToken (Auth a b ) = do
    authenticated <- liftIO $ authenticate a b
    if authenticated then
      liftIO genToken
      else throwError err403 {errBody = "Authentication Failure"}

aApi :: Proxy AuthAPI
aApi = Proxy

app1 :: Application
app1 = serve aApi server

checkCredentials :: Maybe BS.ByteString -> BS.ByteString -> Bool
checkCredentials Nothing _     = False
checkCredentials (Just b) pswd = verifyPassword pswd b

authenticate :: String -> String -> IO Bool
authenticate uname pswd = do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        x <- get (BS.pack uname)
        case x of
          (Left _)  -> return False
          (Right n) -> return $ checkCredentials n $ BS.pack pswd
