module Horse.IO
( -- * staging area
  loadStagingArea
, writeStagingArea

-- * commits
, writeCommit
, loadCommit
) where

import Prelude
import qualified Prelude (show, init, log)

import qualified System.IO as IO
import qualified System.Directory as Dir

import qualified Horse.Commands.Plumbing as Plumbing
import qualified Horse.Filesys as Filesys
import Horse.Types

import Data.Serialize

import Data.Either.Unwrap

import Data.ByteString as ByteString hiding (putStrLn, map)

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Control.Applicative

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

import Data.Default

import Data.Maybe (fromJust)

-- * staging area

loadStagingArea :: IO StagingArea
loadStagingArea =
    fromRight <$> decode <$> ByteString.readFile Filesys.stagingAreaPath

writeStagingArea :: StagingArea -> IO ()
writeStagingArea
    = ByteString.writeFile Filesys.stagingAreaPath
    . encode

writeCommit :: Commit -> Hash -> IO ()
writeCommit commit key = do
    db <- DB.open Filesys.commitsPath def
    DB.put db def key (encode commit)
    DBInternal.unsafeClose db -- TODO

-- TODO: error propagation
loadCommit :: Hash -> IO Commit
loadCommit key = do
    db <- DB.open Filesys.commitsPath def
    commit <- DB.get db def key
    DBInternal.unsafeClose db
    return $ fromRight . decode . fromJust $ commit
