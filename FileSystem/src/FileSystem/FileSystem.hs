{-# LANGUAGE OverloadedStrings #-}

module FileSystem ( deleteFile, insertFile, updateFile
                  , SqlCommand, selectFile, file
                  , newFile) where

import qualified Data.ByteString                    as BS
import           Data.Int
import           Database.MySQL.Simple
import           Database.MySQL.Simple.QueryParams
import           Database.MySQL.Simple.QueryResults
import           Database.MySQL.Simple.Result
import           Utils.Data.File

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
insertFile (File name version fileId bytes) = sqlCmd "insert into files (name,version,fileId,bytes) values (?,?,?,?)" (name,version,fileId,bytes)

newFile :: FileID -> SqlCommand
newFile p = sqlCmd "insert into files (name,version,fileId,bytes) values ('',0,?,'')" [p]

deleteFile :: FileID -> SqlCommand
deleteFile p = sqlCmd "DELETE FROM files WHERE files.fileId=?" [p]

-- | update file in the database
updateFile :: File -> SqlCommand
updateFile (File name version fileId bytes) = sqlCmd "update files set files.name=?, files.version=files.version+1, files.bytes=? where files.fileId=?" (name,bytes,fileId)

selectAllFiles :: SqlQuery [File]
selectAllFiles = sqlQuery_ "select name, version, fileid, bytes from files"

selectFile :: [String] -> SqlQuery [File]
selectFile p = sqlQuery "select name, version, fileid, bytes from files where fileid=?" p

file :: (String,Int, FileID, BS.ByteString) -> File
file (name,version, id, bytes) = File name version id bytes
