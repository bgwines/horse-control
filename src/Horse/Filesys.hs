module Horse.Filesys
( -- * paths
  rootPath
, headPath
, stagingAreaPath
, diffsPath
, commitsPath

-- * file contents
, headInitialContents
, stagingAreaInitialContents

-- * lists
, directories
, databasePaths
, serializationPathsAndInitialContents
) where

import Horse.Types as Types

import Data.ByteString as ByteString

import Data.Serialize

import Data.Default

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

-- * file contents

headInitialContents :: ByteString
headInitialContents = encode $ Head { headHash = def }

stagingAreaInitialContents :: ByteString
stagingAreaInitialContents = encode
    $ StagingArea { files = def }

-- * lists

directories :: [FilePath]
directories = [rootPath, diffsPath, commitsPath]

databasePaths :: [FilePath]
databasePaths = [diffsPath, commitsPath]

serializationPathsAndInitialContents :: [(FilePath, ByteString)]
serializationPathsAndInitialContents =
    [ (headPath, headInitialContents)
    , (stagingAreaPath, stagingAreaInitialContents) ]
