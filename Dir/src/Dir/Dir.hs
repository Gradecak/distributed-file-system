module Dir (getF, newFile , move, listDir, makeRelative) where

import           Data.Foldable      (forM_)
import           Data.Random
import           Data.Random.Extras (sample)
import           Data.Random.RVar
import           System.Directory   as Dir
import           System.FilePath    ((</>))
import qualified System.IO          as IO
import qualified System.IO.Strict   as S.IO
import           Utils.Data.File
import Control.Exception

-- | In order to ensure that the core of the host file system is not
-- | effected by the 'shadow file system'. we prepend a '.' to the filepath.
-- | making the request relative to the directory that this service
-- | is ran from.
makeRelative :: FilePath -> FilePath
makeRelative = ('.':)

-- | Returns location of a remote file from our 'shadow' filesystem
-- | and moves file just served to end of queue for round robin load balancing
getF :: FilePath -> IO (Maybe FileHandle)
getF path = do
    f <- try (S.IO.readFile path) :: IO (Either IOError String)
    case f of
      (Left _ )       -> return Nothing
      (Right content) -> do
          let (x:xs) = read content :: [FileHandle]
          writeFile path $ show (xs ++ [x])
          return $ Just x

newFile :: FilePath -> [(String,Int)] -> IO FileHandle
newFile path servers = do
    fs <- replicationCandidates servers -- pick the servers that the file should be replicated on
    let handles = genFileHandles path fs
        shadowPath = makeRelative path
    writeFile shadowPath (show handles)
    return $ head handles

replicationCandidates :: [(String,Int)] -> IO [(String,Int)]
replicationCandidates servers = runRVar (Data.Random.Extras.sample 2 servers) StdRandom

genFileHandles :: FilePath -> [(String,Int)] -> [FileHandle]
genFileHandles path = map (\addr -> FileHandle path addr)

-- | List the contents of a directory.
-- | Returns: Just [FilePath] if given path is a directory
-- | Returns: Nothing if given path is a file
listDir :: FilePath -> IO (Maybe [FilePath])
listDir path = do
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
