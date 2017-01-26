{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Token (Token(..), InternalToken) where

import           Data.Aeson
import           GHC.Generics

-- | Client tokens contain the randomly generated string,
-- | the expiry time of the token (in seconds) and the source ip
-- | that the token is valid for. Meaning that the token can only be used
-- | on the machine that requested it, or though some naughty ip spoofing
data Token = Token { token    :: String
                   , expiry   :: String
                   , sourceIp :: String
                   } deriving (Show, Eq, Ord, Generic, FromJSON, ToJSON)

-- | internal tokens have no expiry as they last for the duration of the
-- | auth server up time.
-- |
type InternalToken = String
