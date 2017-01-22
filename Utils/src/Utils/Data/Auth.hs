{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Utils.Data.Auth (Auth(..)) where

import           Data.Aeson
import           GHC.Generics

data Auth = Auth { username :: String
                 , password :: String
                 } deriving (Generic, FromJSON, ToJSON)
