{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
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

-- imports

import Prelude hiding (show, init, log)

import GHC.Generics

-- qualified imports

import qualified System.IO as IO
import qualified System.Directory as Dir

import qualified Data.Hex as Hex

import qualified Crypto.Hash.SHA256 as SHA256

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

-- imported functions

import Data.Serialize (Serialize)

import Data.Default (Default, def)
import Data.ByteString (ByteString, empty)

import Data.Maybe (fromJust)
import Data.Either.Unwrap (fromLeft, fromRight)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Text.Printf (printf)

import Control.Monad ((>>=), return)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)

type Email = String

type Hash = ByteString

instance Default Hash where
    def :: Hash
    def = empty

type Diff = [(FilePath, String, String)]

-- `toGregorian . utctDay` on a date
type Date = (Integer, Int, Int)

data Author = Author {
    name :: String,
    email :: String
} deriving (Eq, Show, Generic)

instance Serialize Author

data Commit = Commit {
    author :: (String, Email),
    date :: Date,
    hash :: Hash,
    parentHash :: Hash,
    secondaryParentHash :: Maybe Hash,
    diffWithPrimaryParent :: Diff,
    message :: String
} deriving (Eq, Show, Generic)

instance Serialize Commit

data Head = Head {
    headHash :: Hash
} deriving (Eq, Show, Generic)

instance Serialize Head

-- why doesn't Generic handle this?
instance Default Head where
    def :: Head
    def = Head { headHash = def }

data StagingArea = StagingArea {
    modsOrAdds :: [FilePath],
    deletions :: [FilePath]
} deriving (Eq, Show, Generic)

instance Serialize StagingArea

instance Default StagingArea where
    def :: StagingArea
    def = StagingArea { modsOrAdds = def, deletions = def }
