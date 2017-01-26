{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module FileSystem.Gossip ( newGossipCache, inCache,
                           cacheFile, startGossip,
                           disseminateFile, staleFile, GossipTable,
                         gossipCandidates) where

import File.API (fileAPI)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Data.Proxy
import Servant.API
import Data.Random.Extras (sample)
import Data.Random.RVar
import Data.Random
import System.Clock
import Servant.Client
import FileSystem (selectFile)
import Data.Cache
import Token
import Utils.Data.File
import Servant.Client

data Propagated = Propagated

type GossipTable = Cache FilePath Propagated

_ :<|> _ :<|> _ :<|> goss = client fileAPI

-- | Create a new cache where we can keep track of our gossip protocol
-- | forwarding, entries in the gossip cache will expire after 15 minutes
newGossipCache :: IO GossipTable
newGossipCache = newCache (Just $ TimeSpec{sec=900,nsec=0})

-- | check if a filepath is in the cache (ie it has been 'gossiped about'
-- | before)
inCache :: FilePath -> GossipTable -> IO Bool
inCache path table = do
    x <- Data.Cache.lookup table path
    return $ case x of
      Nothing -> True
      _       -> False

-- | a comparison of file versions.
staleFile :: File -> File -> Bool
staleFile f1 f2 = if (version f1) > (version f2)
                  then True
                  else False

cacheFile :: File -> GossipTable -> IO ()
cacheFile file table = insert table (filepath file) Propagated

disseminateFile :: File -> [(String,Int)] -> InternalToken -> IO ()
disseminateFile file dest tok = do
    manager <- newManager defaultManagerSettings
    endpts <- gossipCandidates dest
    let destinations = genDestinations endpts manager
    mapM_ (runClientM (goss (Just tok) file)) destinations

startGossip :: File -> GossipTable -> [(String,Int)] -> InternalToken -> IO ()
startGossip file table endpts tok = do
    cached <- inCache (filepath file) table
    if cached
       then return ()
       else do
        cacheFile file table
        disseminateFile file endpts tok

genDestinations :: [(String,Int)] -> Manager -> [ClientEnv]
genDestinations x m = map (\(ip,port) -> ClientEnv m (BaseUrl Http ip port "")) x


gossipCandidates :: [(String,Int)] -> IO ([(String,Int)])
gossipCandidates fs = runRVar (Data.Random.Extras.sample 2 fs ) StdRandom
