module Token.Store (insert, Token.Store.lookup, Token.Store.lookupB) where

import Token
import Database.Redis
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS (ByteString, pack)


insert :: Token -> Connection -> IO ()
insert t conn = void $
    runRedis conn $ do
        let key = BS.pack $ token t
        _ <- set key (BS.pack $ sourceIp t)
        expire key (read $ expiry t)

lookup :: Token -> Connection -> IO Bool
lookup t conn =  runRedis conn $ do
    x <- get (BS.pack $ token t)
    return $ case x of
      Right (Just _) -> True
      _              -> False

lookupB :: BS.ByteString -> Connection -> IO Bool
lookupB t conn = runRedis conn $ do
    x <- get t
    return $ case x of
      Right (Just _) -> True
      _              -> False
