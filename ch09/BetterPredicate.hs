import           Control.Exception (bracket, handle, SomeException(..))
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

ignoreException :: SomeException -> IO (Maybe Integer)
ignoreException _ = return Nothing

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ignoreException $
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

saferFileSize path = handle ignoreException $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)

type InfoP a =  FilePath        -- path to directory entry
             -> Permissions     -- permissions
             -> Maybe Integer   -- file size (Nothing if not file)
             -> UTCTime         -- last modified
             -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)
