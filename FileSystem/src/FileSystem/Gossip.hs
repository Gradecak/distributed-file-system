{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module FileSystem.Gossip () where

import FileServer.API
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Data.Proxy
import Servant.API
import Servant.Client

fileAPI :: Proxy FileAPI
fileAPI = Proxy
