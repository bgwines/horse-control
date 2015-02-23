{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

module Horse.Commands.Porcelain
( -- * Basic commands
  Horse.Commands.Porcelain.add
, Horse.Commands.Porcelain.rm
, Horse.Commands.Porcelain.checkout
, Horse.Commands.Porcelain.commit
, Horse.Commands.Porcelain.diff
, Horse.Commands.Porcelain.init
, Horse.Commands.Porcelain.log
, Horse.Commands.Porcelain.status
, Horse.Commands.Porcelain.show
) where

import Prelude
import qualified Prelude (show, init, log)

import qualified System.IO as IO
import qualified System.Directory as Dir

import Data.Serialize

import qualified Data.Hex as Hex

import Data.Default

import Text.Printf (printf)

import Data.Either.Unwrap

-- TODO
import Data.ByteString as ByteString hiding (putStrLn, map, head, concatMap)

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Control.Applicative ((<$>), (<*>))

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

import qualified Crypto.Hash.SHA256 as SHA256

import qualified Horse.Commands.Plumbing as Plumbing
import qualified Horse.Filesys as Filesys
import qualified Horse.IO as HIO
import Horse.Types as Types

data Flag
    = Add
    | Delete
    | List
    | Number Integer
    | Squash
    | FastForward
    | All
    | Verbose
    | Quiet
    | Recursive
    | Force
    | Message

init :: [String] -> IO ()
init _ = do
    rootDirAlreadyExists <- Dir.doesDirectoryExist Filesys.rootPath

    sequence $ map Filesys.destructivelyCreateDirectory Filesys.directories

    sequence $ map (flip DB.open $ DB.defaultOptions{ DB.createIfMissing = True }) Filesys.databasePaths

    sequence $ map Filesys.createFileWithContents Filesys.serializationPathsAndInitialContents

    currDir <- Dir.getCurrentDirectory
    if rootDirAlreadyExists
        then putStrLn
            $ "Re-initialized existing horse-control repository in"
            ++ currDir ++ "/" ++ Filesys.rootPath
        else putStrLn
            $ "Initialized existing horse-control repository in"
            ++ currDir ++ "/" ++ Filesys.rootPath

    --DBInternal.unsafeClose db
    return ()

status :: [String] -> IO ()
status _ = do
    stagingArea <- HIO.loadStagingArea
    print stagingArea

add :: [String] -> IO ()
add args = do
    stagingArea <- HIO.loadStagingArea
    HIO.writeStagingArea $ stagingArea { modsOrAdds = (modsOrAdds stagingArea) ++ args }

-- TODO: rm from filesystem
rm :: [String] -> IO ()
rm args = do
    stagingArea <- HIO.loadStagingArea
    HIO.writeStagingArea $ stagingArea { deletions = (deletions stagingArea) ++ args }

commit :: [String] -> IO ()
commit args = do
    now <- fmap (toGregorian . utctDay) getCurrentTime
    parentHash <- return def -- TODO
    stagedDiff <- HIO.loadStagingArea >>= getStagedDiff
    -- TODO: coalesce commit-creation somehow?
    let hashlessCommit = Commit {
        author                = ( "Brett Wines"
                                , "bgwines@cs.stanford.edu") -- TODO
        , date                = now
        , hash                = def -- no hash yet since commit
                                    -- hasn't been created
        , parentHash          = parentHash
        , secondaryParentHash = def -- not a merge commit, so
                                    -- no secondary parent
        , Types.diff          = stagedDiff -- TODO: handle more nicely
        , message             = message }

    let commitHash = hashCommit hashlessCommit
    let completeCommit = hashlessCommit { hash = commitHash }

    HIO.writeCommit completeCommit commitHash

    HIO.writeStagingArea (def :: StagingArea)

    writtenCommit <- HIO.loadCommit commitHash
    putStrLn $ "Testing writing of commit: loading written commit: "
        ++ (Prelude.show writtenCommit)

    -- TODO: print
    --     [master d75dc6d] <commit message
    --      1 file changed, 1 insertion(+)
    --      create mode 100644 hi.txt

    where
        message :: String
        message = head args -- TODO

        -- TODO: where does this go?
        hashCommit :: Commit -> Hash
        hashCommit
            = ByteString.take 40
            . Hex.hex
            . SHA256.hash
            . encode

        -- TODO
        getStagedDiff :: StagingArea -> IO Diff
        getStagedDiff stagingArea = return def

checkout :: [String] -> IO ()
checkout args = do
    putStrLn $ "running command \"checkout\" with args " ++ Prelude.show args

diff :: [String] -> IO ()
diff args = do
    putStrLn $ "running command \"diff\" with args " ++ Prelude.show args

log :: [String] -> IO ()
log args = do
    putStrLn $ "running command \"log\" with args " ++ Prelude.show args

show :: [String] -> IO ()
show args = do
    putStrLn $ "running command \"show\" with args " ++ Prelude.show args
