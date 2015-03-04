{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Lower-level commands accessible to users through the CLI
module Horse.Commands.Plumbing
( -- * Basic commands
  diffFiles
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


import Data.Either.Unwrap (fromLeft, fromRight)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Text.Printf (printf)

import Control.Monad ((>>=), return)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)

-- horse-control imports

import Horse.Types

-- | Calculates the difference between two files in the filesystem
diffFiles :: FilePath -> FilePath -> IO Diff
diffFiles a b = return Default.def
