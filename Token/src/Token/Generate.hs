module Token.Generate (genToken) where

import           Token
import           Data.DateTime
import           Data.UUID     (toString)
import           Data.UUID.V4  (nextRandom)

genToken :: IO Token
genToken = do
    t <- nextRandom
    e <- expiryDate
    return $ Token (toString t) e "192.168.0.1"

expiryDate :: IO DateTime
expiryDate = fmap (addMinutes' 30) getCurrentTime
