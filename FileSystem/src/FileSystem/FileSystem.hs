{-# LANGUAGE OverloadedStrings #-}

module FileSystem where

import Utils.Data.File
import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.Result
import Database.MySQL.Simple.QueryParams
import Data.Int
import qualified Data.ByteString as BS

-- | making File an instance of QueryResult will allow us to query our
-- | database tables directly to our Haskell File datatype
instance QueryResults File where
 convertResults [fa,fb,fc,fd] [va,vb,vc,vd] = File a b c d
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
          d = convert fd vd
 convertResults fs vs  = convertError fs vs 2

-- | Some handy aliases to make our typesignitures shorter
type SqlQuery a = Connection -> IO a
type SqlCommand = Connection -> IO Int64

-- | SQL query with paramaters
sqlQuery :: (QueryParams q, QueryResults r) => Query -> q -> Connection -> IO [r]
sqlQuery q vs conn = query conn q vs

-- | SQL query with NO paramaters
sqlQuery_ :: QueryResults r => Query -> Connection -> IO [r]
sqlQuery_ q conn = query_ conn q

-- | SQL command with paramaters
sqlCmd :: QueryParams q => Query -> q -> Connection -> IO Int64
sqlCmd q vs conn = execute conn q vs

-- | SQL command with NO paramaters
sqlCmd_ :: Query -> Connection -> IO Int64
sqlCmd_ q conn = execute_ conn q

-- | a 'bind-like' operator for stringing our queries together
(>>>) :: SqlQuery a -> SqlQuery b -> SqlQuery b
(>>>) q1 q2 conn = do
  q1 conn
  q2 conn

-- | insert file into our table
insertFile :: File -> SqlCommand
insertFile (File name version path bytes) = sqlCmd "insert into files (name, version, filepath, bytes) values (?, ?, ?, ?)" (name,version,path,bytes)

selectAllFiles :: SqlQuery [File]
selectAllFiles = sqlQuery_ "select name, version, filepath, bytes from files"

selectFile :: [String] -> SqlQuery [File]
selectFile p = sqlQuery "select name, version, filepath, bytes from files where filepath=?" p

file :: (String,Int, String, BS.ByteString) -> File
file (name,version, path, bytes) = File name version path bytes
