module Token.Generate (genToken, genInternalToken, genFileID) where

import           Token
import           Data.UUID     (toString)
import           Data.UUID.V4  (nextRandom)

-- | generate a new client token using the UUID library
genToken :: String -> IO Token
genToken srcIP = do
    t <- nextRandom
    return $ Token (toString t) "3600" srcIP

-- | generate a new internal token (to be used for inter service communication)
genInternalToken :: IO InternalToken
genInternalToken = fmap toString nextRandom

genFileID :: IO String
genFileID = genInternalToken
