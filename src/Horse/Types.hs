{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Data types used by the implementation.
module Horse.Types(
-- * Records and their accessor functions
  UserInfo(..)
, Commit(..)

, StagingArea(..)
, files
, mapStagingArea
, isEmpty

, Config(..)

, Status(..)

, Verbosity(..)

, CommitHasher(..)

-- * Aliases
, EmailAddress
, Hash
, Date
, Error
) where

-- imports

import Prelude hiding (init, log)

import GHC.Generics

import qualified Data.Hex as Hex

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Filediff as FD
import qualified Filediff.Types as FD

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)

import qualified Crypto.Hash.SHA256 as SHA256

-- imported functions

import Data.Char (toLower)

import Data.Serialize (Serialize, put, get)
import qualified Data.Serialize as Serialize (encode)

import Data.Default (Default, def)
import Data.ByteString (ByteString, empty)

instance Serialize T.Text where
    put = put . TE.encodeUtf8
    get = fmap TE.decodeUtf8 get

instance Serialize (FD.ListDiff T.Text)
instance Serialize FD.FileChange
instance Serialize FD.Filediff
instance Serialize FD.Diff

-- | Data type for any kind of error.
type Error = String

-- | An e-mail address.
type EmailAddress = String

-- | The hash type used for hashing commits and diffs.
type Hash = ByteString

instance Default Hash where
    def :: Hash
    def = empty

-- `toGregorian . utctDay` on a date
-- | Date information: (year, month, day).
type Date = (Integer, Int, Int)

-- | A record representing information about the user.
data UserInfo = UserInfo {
    name :: String,
    email :: String
} deriving (Eq, Generic)

instance Serialize UserInfo

instance Show UserInfo where
    show :: UserInfo -> String
    show (UserInfo userName userEmail) =
        userName ++ " <" ++ userEmail ++ ">"

-- | A commit record.
data Commit = Commit {
    author :: UserInfo,
    date :: Date,
    hash :: Hash,
    parentHash :: Maybe Hash,
    diffWithPrimaryParent :: FD.Diff,
    message :: String
} deriving (Eq, Show, Generic)

instance Serialize Commit

-- | User-specific configuration information, analogous to what's
--   in ~/.gitconfig with Git.
data Config = Config {
    userInfo :: UserInfo
} deriving (Eq, Show, Generic)

instance Serialize Config

-- | The staging area: files with changes to include in an upcoming
--   commit.
data StagingArea = StagingArea {
    adds :: [FilePath],
    mods :: [FilePath],
    dels :: [FilePath]
} deriving (Eq, Show, Generic)

-- | Maps the given function over the list of adds, the list of mods,
--   and the list of dels.
mapStagingArea :: ([FilePath] -> [FilePath]) -> StagingArea -> StagingArea
mapStagingArea f (StagingArea adds mods dels) =
    StagingArea (f adds) (f mods) (f dels)

instance Serialize StagingArea

-- | Whether the specified staging area does not store any files.
isEmpty :: StagingArea -> Bool
isEmpty (StagingArea [] [] []) = True
isEmpty _ = False 

-- | Gets all files staged (be they added, modified, or deleted files).
files :: StagingArea -> [FilePath]
files (StagingArea adds mods dels) = adds ++ mods ++ dels

instance Default StagingArea where
    def :: StagingArea
    def = StagingArea { adds = def, mods = def, dels = def }

-- | The current status of the repository; essentially, the difference
--   between the current state of the filesystem, and the state it had
--   in HEAD.
data Status = Status {
    stagingArea :: StagingArea,
    unstagedFiles :: [FilePath]
} deriving (Eq, Show)

instance Default Status where
    def :: Status
    def = Status def def

-- | Degree of printing / logging to be executed by exposed commands.
data Verbosity = Quiet | Normal | Verbose deriving (Eq, Show, Read)

-- | Encapsulation of hashing algorithm. Decomposed for mocking purposes
--   for testing. Is an instance of 'Default'; use that unless testing.
data CommitHasher = CommitHasher {
    hashingAlg :: (Commit -> Hash)
}

instance Default CommitHasher where
    def :: CommitHasher
    def = CommitHasher
        $ BS.pack
        . map (BS.c2w . toLower . BS.w2c)
        . BS.unpack
        . BS.take 40
        . Hex.hex
        . SHA256.hash
        . Serialize.encode
