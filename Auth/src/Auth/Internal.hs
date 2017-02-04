{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Auth.Internal (HandlerData(..), AuthM, ask,
                     generateToken, addNewFS,
                     liftIO, authenticate, addUser) where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar (TVar, readTVarIO)
import           Control.Monad.Reader
import           Crypto.PasswordStore
import qualified Data.ByteString.Char8       as BS
import           Database.Redis
import           Network.HTTP.Client         (Manager, defaultManagerSettings,
                                              newManager)
import           Servant.API
import           Servant.Client
import           Shared.API (_registerFsEndPt, _tokenEndPt)
import           Token                       (InternalToken, Token)
import           Token.Generate
import           Utils.Data.Auth
import           Utils.ReaderHandler

-- core data that all of our handlers will need access to
data HandlerData = Info { fileServers   :: TVar [(String,Int)] -- list of file servers registered
                        , dirServer     :: TVar (String,Int)
                        , transServer   :: TVar (String,Int)
                        , redisCon      :: Connection
                        , internalToken :: String
                        }

-- alias for our transformed ReaderHandler
type AuthM = ReaderHandler HandlerData

-- -- | notify all of the registered services of a new file server
-- notifyNewFS :: (String,Int) -> [(String,Int)] -> InternalToken -> IO ()
-- notifyNewFS newFS services token = queryAPI services ((_registerEndPt $ Just token) newFS)

addNewFS :: (String,Int) -> AuthM (InternalToken, [(String, Int)])
addNewFS newFS = do
    Info{fileServers=f, dirServer=d, internalToken=tok} <- ask
    liftIO $ do
        fs <- readTVarIO f
        ds <- readTVarIO d
        print $ show $ ds:fs
        print "notifying directory"
        queryAPI (ds:fs) (_registerFsEndPt (Just tok) newFS)
        atomically $ modifyTVar' f (newFS:)
        return (tok,fs)

-- | Generate a token and notify all interested services of new valid token,
-- returns the newly generated token
generateToken :: String -> AuthM Token
generateToken ip = do
    Info{internalToken=t, dirServer=d, fileServers=f, transServer=trans} <- ask
    liftIO $ do
        newToken <- genToken ip
        ds       <- readTVarIO d
        fs       <- readTVarIO f
        ts       <- readTVarIO trans
        print (ts:ds:fs)
        queryAPI (ts:ds:fs) (_tokenEndPt (Just t) newToken)
        return newToken

authenticate :: Connection -> String -> String -> IO Bool
authenticate conn uname pswd = runRedis conn $ do
    x <- get (BS.pack uname)
    return $ case x of
      Right (Just y) -> verifyPassword (BS.pack pswd) y
      _              -> False

addUser :: Auth -> Connection -> IO ()
addUser (Auth uname pswd) conn = do
    pswdHash <- makePassword (BS.pack pswd) 17
    void $ runRedis conn $ set (BS.pack uname) pswdHash

{- Module Internal Functions -}
-- | Convert the destingations from [(String,Int)] format to a
-- | Servant-Client usable format [ClientEnv]
genDestinations :: [(String,Int)] -> Manager -> [ClientEnv]
genDestinations x m = map (\(ip,port) -> ClientEnv m (BaseUrl Http ip port "")) x

-- | a HOF for querying a set of endpoints
-- | given the Endpoints.                       -> [(String,Int)]
-- | and the function for interacting with api. -> (ClientM a)
queryAPI :: [(String,Int)] -> ClientM b -> IO ()
queryAPI dest endpt= do
    manager <- newManager defaultManagerSettings
    let destinations = genDestinations dest manager
    mapM_ (runClientM endpt) destinations
