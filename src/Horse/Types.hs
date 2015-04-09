{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Data types used in horse-config implementation
module Horse.Types(
-- * Records and their accessor functions
  UserInfo(..)
, Commit(..)

, StagingArea(..)
, files

, Config(..)

, Status(..)

, Verbosity(..)

-- * Aliases
, Email
, Hash
, Relative(..)
, Date
, Error
) where

-- imports

import Prelude hiding (init, log)

import GHC.Generics

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Filediff as FD
import qualified Filediff.Sequence as FD
import qualified Filediff.Types as FD

-- imported functions

import Data.Serialize (Serialize, put, get)

import Data.Default (Default, def)
import Data.ByteString (ByteString, empty)

instance Serialize T.Text where
    put = put . TE.encodeUtf8
    get = fmap TE.decodeUtf8 get

instance Serialize (FD.SeqDiff T.Text)
instance Serialize FD.FileChange
instance Serialize FD.Filediff
instance Serialize FD.Diff

-- | Error type
type Error = String

-- | An e-mail.
type Email = String

-- | The hash type used for hashing commits and diffs.
type Hash = ByteString

-- | Data type to represent references relative to a commit
--   (e.g. ^ or ~ from Git)
data Relative = Parent

instance Default Hash where
    def :: Hash
    def = empty

-- `toGregorian . utctDay` on a date
-- | Date information to be stored in commits.
type Date = (Integer, Int, Int)

-- | A record representing information about the user.
data UserInfo = UserInfo {
    name :: String,
    email :: String
} deriving (Eq, Show, Generic)

instance Serialize UserInfo

-- | A commit object.
data Commit = Commit {
    author :: UserInfo,
    date :: Date,
    hash :: Hash,
    parentHash :: Maybe Hash,
    diffWithPrimaryParent :: FD.Diff,
    message :: String
} deriving (Eq, Show, Generic)

instance Serialize Commit

-- | Custom horse-control configuration, analogous to what's in ~/.gitconfig with Git.
data Config = Config {
    userInfo :: UserInfo
} deriving (Eq, Show, Generic)

instance Serialize Config

-- | The staging area. Doesn't explicitly store the diffs; those are computed on the fly.
data StagingArea = StagingArea {
    adds :: [FilePath],
    mods :: [FilePath],
    dels :: [FilePath]
} deriving (Eq, Show, Generic)

instance Serialize StagingArea

files :: StagingArea -> [FilePath]
files (StagingArea adds mods dels) = adds ++ mods ++ dels

instance Default StagingArea where
    def :: StagingArea
    def = StagingArea { adds = def, mods = def, dels = def }

data Status = Status {
    stagingArea :: StagingArea,
    unstagedFiles :: [FilePath]
} deriving (Eq, Show)

-- | Degree of printing to be executed by commands
data Verbosity = Quiet | Normal | Verbose deriving (Eq, Show, Read)
