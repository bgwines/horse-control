-- | Module for writing and reading from disk / the database
module Horse.IO
( -- * staging area
  loadStagingArea
, writeStagingArea

-- * HEAD
, loadHead
, writeHead

-- * commits
, loadCommit
, writeCommit

-- * config
, loadConfig
, writeConfig
) where

-- imports

import Prelude hiding (show, init, log)

import GHC.Generics

-- qualified imports

import qualified Data.Convertible as Convert

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
import Horse.Filesys as Filesys

-- | Writes a serializable object to a file
writeToFile :: (Serialize.Serialize a) => FilePath -> a -> IO ()
writeToFile filepath
    = ByteString.writeFile filepath
    . Serialize.encode

-- | Loads a serializable object from a file
loadFromFile :: (Serialize.Serialize a) => FilePath -> IO a
loadFromFile filepath = do
    fromRight
    <$> Serialize.decode
    <$> ByteString.readFile filepath

-- * staging area

-- | Loads the staging area from disk
loadStagingArea :: IO StagingArea
loadStagingArea = loadFromFile Filesys.stagingAreaPath

-- | Writes the staging area to disk
writeStagingArea :: StagingArea -> IO ()
writeStagingArea = writeToFile Filesys.stagingAreaPath

-- * HEAD

-- | Loads the object representing HEAD from disk
loadHead :: IO Head
loadHead = loadFromFile Filesys.headPath

-- | Writes the object representing HEAD to disk
writeHead :: Head -> IO ()
writeHead = writeToFile Filesys.headPath

-- * commits

-- | Loads the commit with the given hash from the database
loadCommit :: Hash -> IO (Maybe Commit)
loadCommit key = do
    db <- DB.open Filesys.commitsPath Default.def
    maybeCommit <- DB.get db Default.def key
    DBInternal.unsafeClose db
    -- TODO: something more monadic instead of fromRight
    return $ (fromRight . Serialize.decode) <$> maybeCommit

-- | Writes the commit with the given hash to the database
writeCommit :: Commit -> Hash -> IO ()
writeCommit commit key = do
    db <- DB.open Filesys.commitsPath Default.def
    DB.put db Default.def key (Serialize.encode commit)
    DBInternal.unsafeClose db

-- * config

-- | Loads the object representing user-specified configuration from disk
loadConfig :: IO Config
loadConfig = Filesys.getConfigPath >>= loadFromFile

-- | Writes the object representing user-specified configuration to disk
writeConfig :: Config -> IO ()
writeConfig config = do
    configPath <- Filesys.getConfigPath
    ByteString.writeFile configPath (Serialize.encode config)
