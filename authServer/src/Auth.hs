{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Auth where

import           Crypto.PasswordStore
import           Data.Aeson
import qualified Data.ByteString.Char8              as BS (ByteString, pack)
import           Database.MySQL.Simple
import qualified Database.MySQL.Simple.QueryParams  as QP
import           Database.MySQL.Simple.QueryResults
import           Database.MySQL.Simple.Result
import           GHC.Generics
import           GHC.Word
import           Network.CGI                        (liftIO)
import           Network.Wai
import           Servant
import           System.Environment                 (getEnv)
import           Token                              (Token)
import           Token.Generate                     (genToken)

data User = User { name :: String, hash :: String } deriving (Show)

instance QueryResults User where
  convertResults [fa,fb] [va,vb] = User { name = a, hash = b }
    where a = convert fa va
          b = convert fb vb
  convertResults fs vs  = convertError fs vs 2

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

databaseInf :: IO ConnectInfo
databaseInf = do
    db_ip   <- getEnv "MYSQL_PORT_3306_TCP_ADDR"
    db_port <- getEnv "MYSQL_PORT_3306_TCP_PORT"
    return $ ConnectInfo db_ip (read db_port ::GHC.Word.Word16) "root" "Vukovar91" "auth_db" [] "" Nothing

selectUser :: QP.QueryParams q => q -> Connection -> IO [User]
selectUser q conn = query conn "select * from users where username=?" q

checkCredentials :: [User] -> BS.ByteString -> Bool
checkCredentials [] _       = False
checkCredentials (x:_) pswd = verifyPassword pswd $ BS.pack (hash x)

authenticate :: String -> String -> IO Bool
authenticate uname pswd = do
    db_info <- databaseInf
    db_conn <- connect db_info
    usr     <- selectUser [uname::String] db_conn
    return   $ verifyPassword (BS.pack pswd) $ BS.pack $ hash (Prelude.head usr)
