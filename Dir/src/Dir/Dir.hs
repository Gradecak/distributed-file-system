{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Dir (getF, toFile, openFNew) where

import           Control.Monad    (mzero)
import           Data.Aeson
import           Utils.Data.File
import           GHC.Generics
import           System.Directory as Dir
import qualified System.IO        as IO
import qualified System.IO.Strict as S.IO

-- data FileHandle = FileHandle  { path     :: FilePath
--                               , serverIp :: String
--                               } deriving (Show, Read, Generic, FromJSON, ToJSON)

-- -- type of access required to a file
-- data FileMode = Read | Write | ReadWrite
--     deriving (Show)

-- -- request type for accing a file
-- data FileRequest = Request FilePath FileMode
--     deriving (Show)

-- -- making JSON instance for our FileRequest ADT ---
-- instance FromJSON FileRequest where
--     parseJSON (Object v) = mode <$>
--                            v .: "path" <*>
--                            v .: "mode"
--     parseJSON _          = mzero

-- instance ToJSON FileRequest where
--     toJSON (Request (path) Read)      = object ["path" .= path, "mode" .= String "Read"]
--     toJSON (Request (path) Write)     = object ["path" .= path, "mode" .= String "Write"]
--     toJSON (Request (path) ReadWrite) = object ["path" .= path, "mode" .= String "ReadWrite"]

-- -- helper function for our JSON instance
-- mode :: String -> String -> FileRequest
-- mode path m = Request path $ case m of
--                                 "Write"     -> Write
--                                 "Read"      -> Read
--                                 "ReadWrite" -> ReadWrite

-- -- write a file location to a file in our 'shadow' filesystem
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
