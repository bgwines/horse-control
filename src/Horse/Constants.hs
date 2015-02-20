module Horse.Constants
( rootPath
, diffsPath
, blobsPath
, infoPath
, infoInitialContents
) where

-- dir constants
rootPath :: FilePath
rootPath = ".horse"

diffsPath :: FilePath
diffsPath = rootPath ++ "/diffs"

blobsPath :: FilePath
blobsPath = rootPath ++ "/blobs"

infoPath :: FilePath
infoPath = rootPath ++ "/info"

infoInitialContents :: String
infoInitialContents = "hi"

data Info = Info {
    head :: String,
    stagingArea :: [FilePath]
} deriving (Show)

data UserInfo = UserInfo {
    name :: String,
    email :: String
}