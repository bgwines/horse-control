{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

module Horse.Commands.Plumbing
( -- * Basic commands
  diffFiles
) where

import qualified System.IO as IO
import qualified System.Directory as Dir

import qualified Horse.Filesys as Filesys
import Horse.Types as Types

import Data.Default

import Data.ByteString as ByteString

import qualified Crypto.Hash.SHA256 as SHA256

import GHC.Generics

diffFiles :: FilePath -> FilePath -> IO ByteString
diffFiles a b = return def