module Horse.IO
( -- * staging area
  loadStagingArea
, writeStagingArea
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

import qualified Database.LevelDB as DB

import Data.Default

-- * staging area

loadStagingArea :: IO StagingArea
loadStagingArea =
    fromRight <$> decode <$> ByteString.readFile Filesys.stagingAreaPath

writeStagingArea :: StagingArea -> IO ()
writeStagingArea
    = ByteString.writeFile Filesys.stagingAreaPath
    . encode