module Lock (Lock(..), LockTable, setLockStatus,
             checkLockStatus, set, free, newTable) where

import qualified Data.Map as Map
import  Utils.Data.File

-- | locking indicator
data Lock = Locked
          | Unlocked

-- | the lock table is will map file paths to the file status. (locked/unlocked)
-- | Read attemps will not result in lock changes
-- | Write/ReadWrite attempts will set the status of the 'path'(file) as Locked
type LockTable = Map.Map FilePath Lock

-- | update the status of the 'filepath'
-- | Essentially an alias for Map.insert
setLockStatus :: FilePath -> Lock -> LockTable -> LockTable
setLockStatus = Map.insert

-- | Check the status of a file in our lock table
-- | if a file path is not in the lock table it means nobody has locked
-- | the file in the past so we return 'Unlocked'
checkLockStatus :: FilePath -> LockTable -> Lock
checkLockStatus path table = case Map.lookup path table of
                               Nothing -> Unlocked
                               Just x -> x

-- | Checks the attempted access to file, if Read no lock update is needed
-- | if write/readwrite the file must be locked before proceeding
set :: FileRequest -> LockTable -> LockTable
set (Request _  Read) table  = table
set (Request p _    ) table = setLockStatus p Locked table

-- | Change the status of a file to 'Unlocked'
free :: FilePath -> LockTable -> LockTable
free path t = setLockStatus path Unlocked t

newTable :: LockTable
newTable = Map.empty
