module Token.Generate (genToken) where

import           Token
import           Data.UUID     (toString)
import           Data.UUID.V4  (nextRandom)

genToken :: IO Token
genToken = do
    t <- nextRandom
    return $ Token (toString t) "3600" "192.168.0.1" "0.0.0.0"
