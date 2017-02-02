{-# LANGUAGE OverloadedStrings #-}
module Dir (getF, newFile , move, listDir, makeRelative,rmFile) where

import           Control.Exception
import           Data.Foldable      (forM_)
import           Data.Random
import           Data.Random.Extras (sample)
import           Data.Random.RVar
import           Dir.Client
import           System.Directory   as Dir
import           System.FilePath    ((</>))
import qualified System.IO          as IO
import qualified System.IO.Strict   as S.IO
import           Token              (InternalToken)
import           Token.Generate     (genFileID)
import           Utils.Data.File

-- | In order to ensure that the core of the host file system is not effected by
-- the 'shadow file system'. we prepend a '.' to the filepath. making the
-- request relative to the directory that this service is ran from.
makeRelative :: FilePath -> FilePath
makeRelative = ('.':)

-- | Returns location of a remote file from our 'shadow' filesystem
-- | and moves file just served to end of queue for round robin load balancing
getF :: FilePath -> IO (Maybe FileHandle)
getF path = do
    handles <- readHandles path
    case handles of
      Nothing -> return Nothing
      Just (x:xs) -> do
          writeFile (makeRelative path) $ show (xs ++ [x])
          return $ Just x

-- | reads the [FileHandles] from the provided path, catches IOErrors thrown by
-- readfile and returns (Just [FleHandle]) if read was successful, otherwise
-- Nothing
readHandles :: FilePath -> IO (Maybe [FileHandle])
readHandles path = do
    let shadowPath = makeRelative path
    f <- try (S.IO.readFile shadowPath) :: IO (Either IOError String)
    return $ case f of
      (Left _)        -> Nothing
      (Right content) -> Just (read content)

newFile :: FilePath -> [(String,Int)] -> InternalToken -> IO FileHandle
newFile path servers tok= do
    fs     <- replicationCandidates servers
    fileId <- allocateFileSpace fs tok
    let handles    = genFileHandles fileId fs
        shadowPath = makeRelative path
    writeFile shadowPath (show handles)
    return $ head handles

rmFile :: FilePath -> InternalToken -> IO ()
rmFile path tok = do
    x <- readHandles path
    case x of
      Nothing      -> return ()
      Just handles -> do
          forM_ handles (\(FileHandle id ip) -> deleteFile ip id tok)
          removeFile (makeRelative path)

-- | Sends a create file request to the fileservers instructing them to allocate
-- space for the incoming file, by doing this it allows the directory server to
-- control where the files are replicated. Returns ID of file created
allocateFileSpace :: [(String,Int)] -> InternalToken -> IO FileID
allocateFileSpace dest tok = do
    id <- genFileID
    createFile dest id tok
    return id

replicationCandidates :: [(String,Int)] -> IO [(String,Int)]
replicationCandidates servers = runRVar (Data.Random.Extras.sample 2 servers) StdRandom

genFileHandles :: FileID -> [(String,Int)] -> [FileHandle]
genFileHandles path = map (\addr -> FileHandle path addr)

-- | List the contents of a directory.
-- | Returns: Just [FilePath] if given path is a directory
-- | Returns: Nothing if given path is a file
listDir :: FilePath -> IO (Maybe [FilePath])
listDir path = do
    putStrLn path
    x <- doesDirectoryExist path
    case x of
      True  -> fmap Just (listDirectory path)
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
        let srcPath  =  src </> name
        let destPath = dest </> name
        move (srcPath,destPath)
    removeDirectoryRecursive src       -- after move is done, remove old directory
