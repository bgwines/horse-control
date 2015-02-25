-- | Constants and utility functions for files and 
-- | directories on disk used in the implementation.
module Horse.Filesys
( -- * paths
  rootPath
, headPath
, stagingAreaPath
, diffsPath
, commitsPath
, getConfigPath

-- * lists
, directories
, databasePaths
, serializationPathsAndInitialContents

-- * utility functions
, createFileWithContents
, destructivelyCreateDirectory
) where

-- imports

import Prelude hiding (show, init, log)

import GHC.Generics

-- qualified imports

import qualified System.IO as IO
import qualified System.Directory as Dir

import qualified Data.Hex as Hex
import qualified Data.Default as Default
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString

import qualified Crypto.Hash.SHA256 as SHA256

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

-- imported functions

import Data.Maybe (fromJust)
import Data.Either.Unwrap (fromLeft, fromRight)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Text.Printf (printf)

import Control.Monad ((>>=), return)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)

-- horse-control imports

import Horse.Types

-- * paths

-- | The root path for all (horse-config)-stored data
rootPath :: FilePath
rootPath = ".horse"

-- | The path to where the object representing HEAD is stored
headPath :: FilePath
headPath = rootPath ++ "/HEAD"

-- | The path to where the object representing the staging area is stored
stagingAreaPath :: FilePath
stagingAreaPath = rootPath ++ "/stagingArea"

-- | The path to where diffs are stored
diffsPath :: FilePath
diffsPath = rootPath ++ "/diffs"

-- | The path to where commits are stored
commitsPath :: FilePath
commitsPath = rootPath ++ "/commits"

-- | The path to where the object representing user-specified
-- | configuration information is stored. Returnvalue is wrapped in
-- | the `IO` monad because getting the user's home directory is
-- | a monadic operation.
getConfigPath :: IO FilePath
getConfigPath = Dir.getHomeDirectory >>= return . (flip (++)) "/.horseconfig"

-- * lists

-- | Paths to directories created by the implementation
directories :: [FilePath]
directories = [rootPath, diffsPath, commitsPath]

-- | Paths to databases used in the implementation
databasePaths :: [FilePath]
databasePaths = [diffsPath, commitsPath]

-- | Paths to files used for storing objects, and the initial contents of
-- | those files upon initialization of an empty repository
serializationPathsAndInitialContents :: [(FilePath, ByteString.ByteString)]
serializationPathsAndInitialContents =
    [ (headPath, Serialize.encode $ (Default.def :: Head))
    , (stagingAreaPath, Serialize.encode $ (Default.def :: StagingArea)) ]

-- * utility functions

-- | Creates a file on disk with the specified content
createFileWithContents :: (FilePath, ByteString.ByteString) -> IO ()
createFileWithContents (path, contents) = do
    handle <- IO.openFile path IO.WriteMode
    ByteString.hPutStr handle contents
    IO.hClose handle

-- | Creates a directory on disk at the specified destination, 
-- | destroying one if it was already there
destructivelyCreateDirectory :: FilePath -> IO ()
destructivelyCreateDirectory dir = do
    dirAlreadyExists <- Dir.doesDirectoryExist dir
    if dirAlreadyExists
        then Dir.removeDirectoryRecursive dir
        else return ()
    Dir.createDirectory dir
    return ()
