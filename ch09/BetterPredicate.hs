import           Control.Exception (bracket, handle)
import           Control.Monad     (filterM)
import           Data.Time.Clock   (UTCTime (..))
import           System.Directory  (Permissions (..), getModificationTime,
                                    getPermissions)
import           System.FilePath   (takeExtension)
import           System.IO         (IOMode (..), hClose, hFileSize, openFile)

-- the function we wrote earlier
import           RecursiveContents (getRecursiveContents)

type Predicate =  FilePath      -- path to directory entry
               -> Permissions   -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> UTCTime       -- last modified
               -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\_ -> return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]

betterFind p path = getRecursiveContents path >>= filterM check
   where check name = do
           perms <- getPermissions name
           size <- getFileSize name
           modified <- getModificationTime name
           return (p name perms size modified)

simpleFileSize :: FilePath -> IO Integer

simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

saferFileSize :: FilePath -> IO (Maybe Integer)

saferFileSize path = handle (\_ -> return Nothing) $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)
