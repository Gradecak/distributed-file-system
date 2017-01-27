module FileSystem.Gossip ( newGossipCache, inCache,
                           cacheFile, startGossip,
                           disseminateFile, staleFile,
                           GossipTable, gossipCandidates) where

-- provides a key/value store with timeouts
import           Data.Cache

-- allows us to randomly sample a list of fileservers
import           Data.Random
import           Data.Random.Extras  (sample)
import           Data.Random.RVar

import           File.API            (gossipEndPt)
import           FileSystem          (selectFile)
import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import           Servant.API
import           Servant.Client
import           Servant.Client
import           System.Clock
import           Token
import           Utils.Data.File

-- | A dummy datatype that we will use as a value for our cache
data Propagated = Propagated

-- | Alias for our Cache
type GossipTable = Cache FilePath Propagated

-- | Create a new cache where we can keep track of our gossip protocol
-- forwarding, entries in the gossip cache will expire after 10 minutes
newGossipCache :: IO GossipTable
newGossipCache = newCache (Just $ TimeSpec{sec=600,nsec=0})

-- | check if a filepath is in the cache (ie it has been 'gossiped about'
-- before)
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
    let destinations = genDestinations manager endpts
    mapM_ (runClientM (gossipEndPt (Just tok) file)) destinations

startGossip :: File -> GossipTable -> [(String,Int)] -> InternalToken -> IO ()
startGossip file table endpts tok = do
    cached <- inCache (filepath file) table
    if cached
       then return ()
       else do
        cacheFile file table
        disseminateFile file endpts tok

genDestinations :: Manager -> [(String,Int)] -> [ClientEnv]
genDestinations m = map (\(ip,port) -> ClientEnv m (BaseUrl Http ip port ""))


gossipCandidates :: [(String,Int)] -> IO ([(String,Int)])
gossipCandidates fs = runRVar (Data.Random.Extras.sample 2 fs ) StdRandom
