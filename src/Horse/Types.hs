{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Horse.Types(
-- * Records
  Author(..)
, Commit(..)
, Head(..)
, StagingArea(..)

-- * Aliases
, Email
, Hash
, Diff
) where

import GHC.Generics

import Data.Default

import Data.Serialize
import Data.ByteString as ByteString

import Data.Time.Clock

type Email = String

type Hash = ByteString

instance Default ByteString where
    def :: Hash
    def = ByteString.empty

type Diff = [(FilePath, String, String)]

-- `toGregorian` on a date
type Date = (Integer, Int, Int)

data Author = Author {
    name :: String,
    email :: String
} deriving (Show, Generic)

instance Serialize Author

data Commit = Commit {
    author :: (String, Email),
    date :: Date,
    hash :: Hash,
    parent :: Hash,
    secondaryParent :: Maybe Hash,
    diff :: Diff
} deriving (Show, Generic)

instance Serialize Commit

data Head = Head {
    headHash :: Hash
} deriving (Show, Generic)

instance Serialize Head

data StagingArea = StagingArea {
    stagedContentDiffHash :: Hash
} deriving (Show, Generic)

instance Serialize StagingArea
