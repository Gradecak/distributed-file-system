module Token.Store (insert, Token.Store.lookup) where


import Token
import Database.Redis
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BS (ByteString, pack)


insert :: Token -> IO ()
insert t = do
    conn <- connect defaultConnectInfo
    void $ runRedis conn $ do
        let key = BS.pack $ token t
        _ <- set key (BS.pack $ sourceIp t)
        expire key (read $ expiry t)

lookup :: Token -> IO Bool
lookup t = do
    conn <- connect defaultConnectInfo
    runRedis conn $ do
        x <- get (BS.pack $ token t)
        case x of
          (Left _)  -> return False
          (Right m) -> return $ existsToken m

existsToken :: Maybe BS.ByteString -> Bool
existsToken Nothing  = False
existsToken (Just _) = True
