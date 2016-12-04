{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Token  where

import           Data.Aeson
import           GHC.Generics

data Token = Token { token    :: String
                   , expiry   :: String
                   , sourceIp :: String
                   } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)
