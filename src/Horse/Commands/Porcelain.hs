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

import qualified Horse.Commands.Plumbing as Plumbing
import qualified Horse.Filesys as Filesys
import qualified Horse.IO as HIO
import Horse.Types

import Data.Serialize

import Data.Either.Unwrap

import Data.ByteString as ByteString hiding (putStrLn, map)

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Control.Applicative

import qualified Database.LevelDB.Base as DB

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

    return ()

status :: [String] -> IO ()
status _ = do
    stagingArea <- HIO.loadStagingArea
    print stagingArea

add :: [String] -> IO ()
add args = do
    stagingArea <- HIO.loadStagingArea
    HIO.writeStagingArea $ stagingArea { modsOrAdds = (modsOrAdds stagingArea) ++ args }

rm :: [String] -> IO ()
rm args = do
    stagingArea <- HIO.loadStagingArea
    HIO.writeStagingArea $ stagingArea { deletions = (deletions stagingArea) ++ args }

commit :: [String] -> IO ()
commit args = do
    putStrLn $ "running command \"commit\" with args " ++ Prelude.show args

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
