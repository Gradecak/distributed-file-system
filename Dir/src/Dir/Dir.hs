{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Dir (getF, toFile, openFNew, move, listDir) where

import           Data.Foldable    (forM_)
import           System.Directory as Dir
import           System.FilePath  ((</>))
import qualified System.IO        as IO
import qualified System.IO.Strict as S.IO
import           Utils.Data.File

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

-- | open a new file in our 'shadow' fileSystem for writing
openFNew :: FilePath -> [FileHandle] -> IO FileHandle
openFNew path (f:fs) = writeFile path (show $ fs++[f]) >> return f

-- | List the contents of a directory.
-- | Returns: Just [FilePath] if given path is a directory
-- | Returns: Nothing if given path is a file
listDir :: FilePath -> IO (Maybe [FilePath])
listDir path = do
    x <- doesDirectoryExist path
    case x of
      True -> fmap Just (listDirectory path)
      False -> return Nothing

-- | move a file or a directory
move :: (FilePath, FilePath) -> IO ()
move p@(src,dest) = do
    x <- doesDirectoryExist src -- check if source is a folder
    if x
       then moveDir p           -- if src is folder
       else do                  -- if src is file
        createDirectoryIfMissing True dest
        copyFileWithMetadata src dest
        removeFile src

-- | recurisvely moves a directory
moveDir :: (FilePath,FilePath) -> IO ()
moveDir (src, dest) = do
    createDirectoryIfMissing True dest -- create destination if not exists
    srcFiles <- listDirectory src      -- get src conents
    forM_ srcFiles $ \name -> do
        let srcPath = src </> name
        let destPath = dest </> name
        move (srcPath,destPath)
    removeDirectoryRecursive src       -- after move is done, remove old directory
