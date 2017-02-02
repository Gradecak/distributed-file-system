module Utils.Client () where

import           Network.HTTP.Client (Manager, defaultManagerSettings,
                                      newManager)
import           Servant.Client
import           Token

_runQuery :: (String,Int) -> Manager -> ClientM b -> IO ()
_runQuery dest httpManager endpt = do
    let destinations = genDestinations dest httpManager
    (runClientM endpt) destinations

runQuery :: (String,Int) -> Manager -> ClientM b -> IO (Maybe b)
runQuery dest httpManager endpt = do
    let destinations = genDestinations dest httpManager
    res <- (runClientM endpt) destinations
    return $ case res of
      Left err       -> Nothing
      Right Nothing  -> Nothing
      Right (Just x) -> Just x
      Right r        -> Just r

-- | Convert the destingations from [(String,Int)] format to a
-- | Servant-Client usable format [ClientEnv]
genDestinations :: [(String,Int)] -> Manager -> [ClientEnv]
genDestinations x m = map (\(ip,port) -> ClientEnv m (BaseUrl Http ip port "")) x
