{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Dir (File(..), getF, toFile) where

import           Data.Aeson
import           GHC.Generics
import qualified System.IO as IO
import qualified System.IO.Strict as S.IO

data File = File  { path     :: FilePath
                  , serverIp :: String
                  } deriving (Show, Read, Generic, FromJSON, ToJSON)

-- write a file location to a file in our 'shadow' filesystem
toFile :: File -> IO ()
toFile f@(File p ip) = appendFile p $ show f

-- returns location of a remote file from our 'shadow' filesystem
-- and moves file just served to end of queue for round robin load balancing
getF :: FilePath -> IO File
getF path = do
    f <- S.IO.readFile path
    let (x:xs) = read f :: [File]
    writeFile path $ show (xs ++ [x])
    return x

--openF :: FilePath -> [String] -> File
