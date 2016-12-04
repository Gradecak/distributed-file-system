{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Auth (Auth(..), authenticate)where

import           Crypto.PasswordStore
import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Database.Redis
import           GHC.Generics

data Auth = Auth { username :: String
                 , password :: String
                 } deriving (Generic, FromJSON)

authenticate :: String -> String -> IO Bool
authenticate uname pswd = do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        x <- get (BS.pack uname)
        return $ case x of
          Right (Just y) -> verifyPassword (BS.pack pswd) y
          _              -> False
