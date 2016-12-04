module Token.Generate (genToken) where

import           Token
import           Data.UUID     (toString)
import           Data.UUID.V4  (nextRandom)

genToken :: String -> IO Token
genToken srcIP = do
    t <- nextRandom
    return $ Token (toString t) "3600" srcIP
