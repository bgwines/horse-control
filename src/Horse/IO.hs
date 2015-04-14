{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper module for writing and reading data from disk.
module Horse.IO
( -- * staging area
  loadStagingArea
, writeStagingArea

-- * HEAD
, loadHeadHash
, writeHeadHash

-- * commits
, loadCommit
, writeCommit

-- * config
, loadConfig
, writeConfig
) where

-- imports

import Prelude hiding (init, log, null)

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

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

import qualified Data.Text.Punycode as Punycode (encode)

-- imported functions

import Data.Maybe (isJust, fromJust)

import Data.Time.Clock (getCurrentTime, utctDay)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO, MonadIO(..))

-- horse-control imports

import Horse.Types
import Horse.Utils
    ( maybeToEither
    , stringToHash
    , putStrLn' )
import qualified Horse.Constants as HC

-- | Writes a serializable object to a file.
writeToFile :: (Serialize.Serialize a) => FilePath -> a -> IO ()
writeToFile filepath
    = ByteString.writeFile filepath
    . Serialize.encode

-- | Loads a serializable object from a file.
loadFromFile :: (Serialize.Serialize a) => FilePath -> EitherT Error IO a
loadFromFile filepath = EitherT $ Serialize.decode <$> ByteString.readFile filepath

-- * staging area

-- | Loads the staging area from disk.
loadStagingArea :: EitherT Error IO StagingArea
loadStagingArea = loadFromFile HC.stagingAreaPath

-- | Writes the staging area to disk.
writeStagingArea :: StagingArea -> IO ()
writeStagingArea = writeToFile HC.stagingAreaPath

-- * HEAD

-- | Loads hash of HEAD from disk. Returns a `Left` if no commits have
--   been made.
loadHeadHash :: EitherT Error IO Hash
loadHeadHash = loadFromFile HC.headHashPath

-- | Writes HEAD's hash to disk.
writeHeadHash :: Hash -> IO ()
writeHeadHash = writeToFile HC.headHashPath

-- * commits

-- | Attempts to load the commit with the given hash from the database.
--   Returns a `Left` if the specified `Hash` doesn't reference a commit
--   or if deserialization fails (which might happen if the database
--   got corrupted).
loadCommit :: Hash -> EitherT Error IO Commit
loadCommit key = do
    maybeCommit <- liftIO $ do
        db <- DB.open HC.commitsPath Default.def
        maybeCommit <- DB.get db Default.def key
        DBInternal.unsafeClose db
        return maybeCommit
    commit <- hoistEither $ maybeToEither loadErrorMessage maybeCommit
    hoistEither $ Serialize.decode commit
    where
        loadErrorMessage :: String
        loadErrorMessage = "Could not fetch commit for key " ++ (show key)

-- | Writes the commit to the database, under the key of its hash.
writeCommit :: Commit -> IO ()
writeCommit commit = do
    db <- DB.open HC.commitsPath Default.def
    DB.put db Default.def (hash commit) (Serialize.encode commit)
    DBInternal.unsafeClose db

-- * config

-- | Loads the object representing user-specified configuration from disk.
--   Returns a `Left` if the specified `Hash` doesn't reference a commit
--   or if deserialization fails (which might happen if the database
--   got corrupted).
loadConfig :: EitherT Error IO Config
loadConfig = (liftIO HC.configPath) >>= loadFromFile

-- | Writes the object representing user-specified configuration to disk.
writeConfig :: Config -> IO ()
writeConfig config = do
    configPath <- HC.configPath
    ByteString.writeFile configPath (Serialize.encode config)
