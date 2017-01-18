{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}

module Auth (Auth(..))where

import           Data.Aeson
import           GHC.Generics

data Auth = Auth { username :: String
                 , password :: String
                 } deriving (Generic, FromJSON)
