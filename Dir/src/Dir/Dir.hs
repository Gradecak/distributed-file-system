{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Dir (getF, toFile, openFNew) where

import           Utils.Data.File
import           System.Directory as Dir
import qualified System.IO        as IO
import qualified System.IO.Strict as S.IO

-- write a file location to a file in our 'shadow' filesystem
toFile :: FileHandle -> IO ()
toFile f@(FileHandle p ip) = appendFile p $ show f

-- returns location of a remote file from our 'shadow' filesystem
-- and moves file just served to end of queue for round robin load balancing
getF :: FilePath -> IO FileHandle
getF path = do
    f <- S.IO.readFile path
    let (x:xs) = read f :: [FileHandle]
    writeFile path $ show (xs ++ [x])
    return x

-- open a new file in our 'shadow' fileSystem for writing
openFNew :: FilePath -> [FileHandle] -> IO FileHandle
openFNew path (f:fs) = writeFile path (show $ fs++[f]) >> return f
