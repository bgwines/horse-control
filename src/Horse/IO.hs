module Horse.IO
( -- * staging area
  loadStagingArea
, writeStagingArea

-- * commits
, writeCommit
, loadCommit
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

import Data.Maybe (fromJust)
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

-- * staging area

loadStagingArea :: IO StagingArea
loadStagingArea
    = fromRight
    <$> Serialize.decode
    <$> ByteString.readFile Filesys.stagingAreaPath

writeStagingArea :: StagingArea -> IO ()
writeStagingArea
    = ByteString.writeFile Filesys.stagingAreaPath
    . Serialize.encode

writeCommit :: Commit -> Hash -> IO ()
writeCommit commit key = do
    db <- DB.open Filesys.commitsPath Default.def
    DB.put db Default.def key (Serialize.encode commit)
    DBInternal.unsafeClose db -- TODO

-- TODO: error propagation
loadCommit :: Hash -> IO Commit
loadCommit key = do
    db <- DB.open Filesys.commitsPath Default.def
    commit <- DB.get db Default.def key
    DBInternal.unsafeClose db
    return $ fromRight . Serialize.decode . fromJust $ commit
