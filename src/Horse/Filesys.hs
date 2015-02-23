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

import Horse.Types as Types

import Data.ByteString as ByteString

import Data.Serialize

import Data.Default

import qualified System.IO as IO
import qualified System.Directory as Dir

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

serializationPathsAndInitialContents :: [(FilePath, ByteString)]
serializationPathsAndInitialContents =
    [ (headPath, encode $ (def :: Head))
    , (stagingAreaPath, encode $ (def :: StagingArea)) ]

-- * utility functions

createFileWithContents :: (FilePath, ByteString) -> IO ()
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