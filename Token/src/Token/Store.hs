<<<<<<< HEAD
module Token.Store (insert, lookup, lookupB) where
=======
module Token.Store (insert, Token.Store.lookup, Token.Store.lookupB) where
>>>>>>> 80e33a929568fa03d7a3234566d250c34aef513a

import Token
import Prelude hiding (lookup)
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
