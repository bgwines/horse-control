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

-- * Aliases
, EmailAddress
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

-- | Data type for any kind of error.
type Error = String

-- | An e-mail address.
type EmailAddress = String

-- | The hash type used for hashing commits and diffs.
type Hash = ByteString

-- | Data type to represent references relative to a commit
--   (e.g. ^ or ~ from Git)
data Relative = Parent

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
        userName ++ " (" ++ userEmail ++ ")" 

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

mapStagingArea :: ([FilePath] -> [FilePath]) -> StagingArea -> StagingArea
mapStagingArea f (StagingArea adds mods dels) =
    StagingArea (f adds) (f mods) (f dels)

instance Serialize StagingArea

isEmpty :: StagingArea -> Bool
isEmpty (StagingArea [] [] []) = True
isEmpty _ = False 
--instance Monoid StagingArea where
--    mempty :: StagingArea
--    mempty = StagingArea [] [] []

--    mappend :: StagingArea -> StagingArea -> StagingArea
--    mappend
--        (StagingArea adds1 mods1 dels1)
--        (StagingArea adds2 mods2 dels2)
--        = StagingArea
--            (adds1 `mappend` adds2)
--            (mods1 `mappend` mods2)
--            (dels1 `mappend` dels2)

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
