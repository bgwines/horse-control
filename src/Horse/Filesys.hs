module Horse.Filesys
( -- * paths
  rootPath
, headPath
, stagingAreaPath
, diffsPath
, commitsPath

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

rootPath :: FilePath
rootPath = ".horse"

headPath :: FilePath
headPath = rootPath ++ "/HEAD"

stagingAreaPath :: FilePath
stagingAreaPath = rootPath ++ "/stagingArea"

diffsPath :: FilePath
diffsPath = rootPath ++ "/diffs"

commitsPath :: FilePath
commitsPath = rootPath ++ "/commits"

-- * lists

directories :: [FilePath]
directories = [rootPath, diffsPath, commitsPath]

databasePaths :: [FilePath]
databasePaths = [diffsPath, commitsPath]

serializationPathsAndInitialContents :: [(FilePath, ByteString.ByteString)]
serializationPathsAndInitialContents =
    [ (headPath, Serialize.encode $ (Default.def :: Head))
    , (stagingAreaPath, Serialize.encode $ (Default.def :: StagingArea)) ]

-- * utility functions

createFileWithContents :: (FilePath, ByteString.ByteString) -> IO ()
createFileWithContents (path, contents) = do
    handle <- IO.openFile path IO.WriteMode
    ByteString.hPutStr handle contents
    IO.hClose handle

destructivelyCreateDirectory :: FilePath -> IO ()
destructivelyCreateDirectory dir = do
    dirAlreadyExists <- Dir.doesDirectoryExist dir
    if dirAlreadyExists
        then Dir.removeDirectoryRecursive dir
        else return ()
    Dir.createDirectory dir
    return ()
