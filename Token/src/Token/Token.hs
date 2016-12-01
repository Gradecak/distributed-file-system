{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Token  where

import           Data.Aeson
import           Data.DateTime
import           GHC.Generics


data Token = Token { token  :: String
                   , expiry :: DateTime
                   , dirIp  :: String
                   } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- genToken :: IO Token
-- genToken = do
--     t <- nextRandom
--     e <- expiryDate
--     return $ Token (toString t) e "192.168.0.1"

-- expiryDate :: IO DateTime
-- expiryDate = fmap (addMinutes' 30) getCurrentTime
