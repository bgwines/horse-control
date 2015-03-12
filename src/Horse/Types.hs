{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Data types used in horse-config implementation
module Horse.Types(
-- * Records
  UserInfo(..)
, Commit(..)
, Head(..)
, StagingArea(..)
, Config(..)

-- * Aliases
, Email
, Hash
, Diff
, Relative(..)
, Date
, Error
) where

-- imports

import Prelude hiding (show, init, log)

import GHC.Generics

-- imported functions

import Data.Serialize (Serialize)

import Data.Default (Default, def)
import Data.ByteString (ByteString, empty)

-- | Error type
type Error = String

-- | An e-mail.
type Email = String

-- | The hash type used for hashing commits and diffs.
type Hash = ByteString
data Relative = Parent

instance Default Hash where
    def :: Hash
    def = empty

-- | The difference between two states of the filesystem.
type Diff = [(FilePath, String, String)]

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
    diffWithPrimaryParent :: Diff,
    message :: String
} deriving (Eq, Show, Generic)

instance Serialize Commit

-- | Custom horse-control configuration, analogous to what's in ~/.gitconfig with Git.
data Config = Config {
    userInfo :: UserInfo
} deriving (Eq, Show, Generic)

instance Serialize Config

-- | Functionally very analogous to Git's HEAD pointer.
data Head = Head {
    headHash :: Hash
} deriving (Eq, Show, Generic)

instance Serialize Head

-- why doesn't Generic handle this?
instance Default Head where
    def :: Head
    def = Head { headHash = def }

-- | The staging area. Doesn't explicitly store the diffs; those are computed on the fly.
data StagingArea = StagingArea {
    adds :: [FilePath],
    mods :: [FilePath],
    dels :: [FilePath]
} deriving (Eq, Show, Generic)

instance Serialize StagingArea

instance Default StagingArea where
    def :: StagingArea
    def = StagingArea { adds = def, mods = def, dels = def }
