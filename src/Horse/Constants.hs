{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Constants used by the implementation; mostly paths around
--   the filesystem.
module Horse.Constants
( -- * paths
  repositoryDataDir
, headHashPath
, stagingAreaPath
, diffsPath
, commitsPath
, configPath
, hashesPath
, branchesPath
, untrackedPathsPath

  -- * lists
, directories
, databasePaths
, serializationPathsAndInitialContents

  -- * values
, defaultBranchName
) where

-- imports

import Data.Maybe

import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Either

-- qualified imports

import qualified Filediff as FD
import qualified Filediff.Types as FD

import qualified Shelly as Sh

import qualified Data.Text as Text

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Filesystem.Path (FilePath)

import qualified Data.Default as Def
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

import qualified Data.Text.Punycode as Punycode (encode)

-- imported functions

import Data.List (foldl', (\\))

import Data.Maybe (isJust, fromJust)

import Data.Time.Clock (getCurrentTime, utctDay)

import Filesystem.Path (parent, null)
import Filesystem.Path.CurrentOS (decodeString, encodeString)

import Control.Applicative
import Control.Monad
import "monad-extras" Control.Monad.Extra (iterateMaybeM)
import Control.Monad.IO.Class (liftIO, MonadIO(..))

-- horse-control imports

import Horse.Types
import Horse.Utils
    ( maybeToEither
    , stringToHash
    , putStrLn'
    , iterateMaybe
    , toMaybe
    , (</>)
    , whenM )

-- * paths

-- | The path (relative to root of repository) of the directory in which
--   is stored data used by the implementation.
repositoryDataDir :: FilePath
repositoryDataDir = ".horse"

-- | The path to where HEAD's hash is stored (relative to root of
--   repository).
headHashPath :: FilePath
headHashPath = repositoryDataDir </> "HEAD"

-- | The path to where the object representing the staging area is stored
--   (relative to root of repository).
stagingAreaPath :: FilePath
stagingAreaPath = repositoryDataDir </> "staging-area"

-- | The path to where diffs are stored (relative to root of repository).
diffsPath :: FilePath
diffsPath = repositoryDataDir </> "diffs"

-- | The path to where commits are stored (relative to root of
--   repository).
commitsPath :: FilePath
commitsPath = repositoryDataDir </> "commits"

-- | The path to where all commits' hashes are stored (relative to root of
--   repository).
hashesPath :: FilePath
hashesPath = repositoryDataDir </> "hashes"

-- | The path to where all branches are stored (relative to root of
--   repository).
branchesPath :: FilePath
branchesPath = repositoryDataDir </> "branches"

-- | The path to where all commits' hashes are stored (relative to root of
--   repository).
untrackedPathsPath :: FilePath
untrackedPathsPath = repositoryDataDir </> "untracked-paths"

-- | The path to where the object representing user-specified
--   configuration information is stored. Returnvalue is wrapped in
--   the `IO` monad because getting the user's home directory is
--   a monadic operation.
configPath :: IO FilePath
configPath = (++) <$> D.getHomeDirectory <*> (return "/.horseconfig")

-- * lists

-- | Paths to directories created by the implementation.
directories :: [FilePath]
directories = [repositoryDataDir, diffsPath, commitsPath]

-- | Paths to databases used in the implementation.
databasePaths :: [FilePath]
databasePaths = [diffsPath, commitsPath]

-- | Paths to files used for storing objects, and the initial contents of
--   those files upon initialization of an empty repository.
serializationPathsAndInitialContents :: [(FilePath, ByteString.ByteString)]
serializationPathsAndInitialContents =
    [ (headHashPath       , Serialize.encode $ (Def.def :: Hash))
    , (stagingAreaPath    , Serialize.encode $ (Def.def :: StagingArea)) 
    , (hashesPath         , Serialize.encode $ (Def.def :: [Hash]))
    , (branchesPath       , Serialize.encode $ (Def.def :: [Branch]))
    , (untrackedPathsPath , Serialize.encode $ (Def.def :: [FilePath])) ]

-- | The default branch. For now, "master"
defaultBranchName :: String
defaultBranchName = "master"
