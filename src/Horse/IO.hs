{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper module for writing and reading data from disk.
module Horse.IO
( -- * staging area
  loadStagingArea
, writeStagingArea

-- * HEAD
, loadHeadHash
, writeHeadHash

-- * hashes
, loadAllHashes

-- * commits
, loadCommit
, writeCommit

-- * config
, loadConfig
, writeConfig

-- * untracking
, loadUntrackedPaths
, writeUntrackedPaths

-- * branches
, loadAllBranches
, writeAllBranches

-- * assorted
, loadHistory
, commitsHaveBeenMade
) where

-- imports

import Prelude hiding (init, log, null)

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO, MonadIO(..))

-- qualified imports

import qualified Filediff as FD
import qualified Filediff.Types as FD

import qualified Shelly as Sh

import qualified Data.Text as Text

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

import qualified Data.Text.Punycode as Punycode (encode)

import "monad-extras" Control.Monad.Extra (iterateMaybeM)

-- imported functions

import Data.List (nub, (\\))
import Data.Maybe (isJust, fromJust)

import Data.Time.Clock (getCurrentTime, utctDay)

-- horse-control imports

import Horse.Types
import Horse.Utils (note, hushT)
import qualified Horse.Constants as HC
import qualified Horse.Filesystem as HF

-- | Writes a serializable object to a file.
writeToFile :: (Serialize.Serialize a) => FilePath -> a -> IO ()
writeToFile filepath
    = ByteString.writeFile filepath
    . Serialize.encode

-- | Loads a serializable object from a file.
loadFromFile :: (Serialize.Serialize a) => FilePath -> EIO a
loadFromFile filepath = EitherT $ Serialize.decode <$> ByteString.readFile filepath

-- * staging area

-- | Loads the staging area from disk.
loadStagingArea :: EIO StagingArea
loadStagingArea = loadFromFile HC.stagingAreaPath

-- | Writes the staging area to disk.
writeStagingArea :: StagingArea -> IO ()
writeStagingArea = writeToFile HC.stagingAreaPath

-- * HEAD

-- | Loads hash of HEAD from disk.
loadHeadHash :: EIO Hash
loadHeadHash = loadFromFile HC.headHashPath

-- | Writes HEAD's hash to disk.
writeHeadHash :: Hash -> IO ()
writeHeadHash = writeToFile HC.headHashPath

-- * commits

-- | Attempts to load the commit with the given hash from the database.
--   Returns a `Left` if the specified `Hash` doesn't reference a commit
--   or if deserialization fails (which might happen if the database
--   got corrupted).
loadCommit :: Hash -> EIO Commit
loadCommit key = do
    maybeCommit <- liftIO $ do
        db <- DB.open HC.commitsPath Default.def
        maybeCommit <- DB.get db Default.def key
        DBInternal.unsafeClose db
        return maybeCommit
    commit <- hoistEither $ note loadErrorMessage maybeCommit
    hoistEither $ Serialize.decode commit
    where
        loadErrorMessage :: String
        loadErrorMessage = "Could not fetch commit for key " ++ show key ++ "."

-- | Writes the commit to the database, under the key of its hash.
writeCommit :: Commit -> EIO ()
writeCommit commit = do
    writeHash (hash commit)
    liftIO $ do
        db <- DB.open HC.commitsPath Default.def
        DB.put db Default.def (hash commit) (Serialize.encode commit)
        DBInternal.unsafeClose db

-- * hashes

-- | Loads the hashes of all commits ever made in the repo.
loadAllHashes :: EIO [Hash]
loadAllHashes = loadFromFile HC.hashesPath

-- | Writes the specified hash to the list of hashes of every commit ever
--   made in the repo.
writeHash :: Hash -> EIO ()
writeHash hash
    = liftIO HF.assertCurrDirIsRepo
    >> loadAllHashes
    >>= liftIO . writeToFile HC.hashesPath . (:) hash

-- * config

-- | Loads the object representing user-specified configuration from disk.
--   Returns a `Left` if the specified `Hash` doesn't reference a commit
--   or if deserialization fails (which might happen if the database
--   got corrupted).
loadConfig :: EIO Config
loadConfig
    = liftIO HF.assertCurrDirIsRepo
    >> liftIO HC.configPath
    >>= loadFromFile

-- | Writes the object representing user-specified configuration to disk.
writeConfig :: Config -> IO ()
writeConfig config = do
    HF.assertCurrDirIsRepo

    configPath <- HC.configPath
    ByteString.writeFile configPath (Serialize.encode config)

-- * untracking

loadUntrackedPaths :: EIO [FilePath]
loadUntrackedPaths = do
    liftIO HF.assertCurrDirIsRepo
    map ByteString8.unpack <$> loadFromFile HC.untrackedPathsPath

-- | Loads the hashes of all commits ever made in the repo.
writeUntrackedPaths :: [FilePath] -> IO ()
writeUntrackedPaths paths = do 
    HF.assertCurrDirIsRepo
    writeToFile HC.untrackedPathsPath
        . map ByteString8.pack
        $ paths

loadAllBranches :: EIO [Branch]
loadAllBranches
    = liftIO HF.assertCurrDirIsRepo
    >> loadFromFile HC.branchesPath

writeAllBranches :: [Branch] -> IO ()
writeAllBranches branches
    = liftIO HF.assertCurrDirIsRepo
    >> writeToFile HC.branchesPath branches

--writeBranch :: Branch -> EIO ()
--writeBranch branch
--    = liftIO HF.assertCurrDirIsRepo
--    >> loadAllBranches
--    >>= liftIO . writeToFile HC.branchesPath . (:) branch

--removeStoredBranch :: Branch -> EIO ()
--removeStoredBranch branch
--    = liftIO HF.assertCurrDirIsRepo
--    >> loadAllBranches
--    >>= liftIO . writeToFile HC.branchesPath . (flip (\\) $ [branch])

-- | Loads the history from a given commit, all the way back
--   to the start. Returns in reverse order (latest commit at front).
loadHistory :: Commit -> EIO [Commit]
loadHistory commit = do
    liftIO HF.assertCurrDirIsRepo
    liftIO
        . fmap ((:) commit)
        . iterateMaybeM (runMaybeT . loadParent)
        $ commit
    where
        -- | Attempts to load the parent commit for a given commit.
        loadParent :: Commit -> MaybeT IO Commit
        loadParent commit =
            if hasParent commit
                then hushT
                    . loadCommit
                    . fromJust
                    . parentHash
                    $ commit
                else MaybeT $ return Nothing

-- | Identifies whether any commits have been made in the current
--   repository.
commitsHaveBeenMade :: EIO Bool
commitsHaveBeenMade = (/=) Default.def <$> loadHeadHash
