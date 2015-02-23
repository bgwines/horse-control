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

import qualified Database.LevelDB as DB

init :: [String] -> IO ()
init _ = do
    rootDirAlreadyExists <- Dir.doesDirectoryExist Filesys.rootPath

    sequence $ map destructivelyCreateDirectory Filesys.directories

    -- sequence $ map (flip DB.open $ DB.defaultOptions{ DB.createIfMissing = True }) Filesys.databasePaths

    sequence $ map createFileWithContents Filesys.serializationPathsAndInitialContents

    currDir <- Dir.getCurrentDirectory
    if rootDirAlreadyExists
        then putStrLn
            $ "Re-initialized existing horse-control repository in"
            ++ currDir ++ "/" ++ Filesys.rootPath
        else putStrLn
            $ "Initialized existing horse-control repository in"
            ++ currDir ++ "/" ++ Filesys.rootPath

    return ()
    where
        createFileWithContents :: (FilePath, ByteString) -> IO ()
        createFileWithContents (path, contents) = do
            handle <- IO.openFile path IO.WriteMode
            ByteString.hPutStr handle contents
            IO.hClose handle

        destructivelyCreateDirectory :: FilePath -> IO ()
        destructivelyCreateDirectory dir = do
            dirAlreadyExists <- Dir.doesDirectoryExist dir
            if dirAlreadyExists
                then Dir.removeDirectoryRecursive dir
                else return ()
            Dir.createDirectory dir
            return ()

status :: [String] -> IO ()
status [] = do
    stagingArea <- HIO.loadStagingArea
    print stagingArea
status args = do
    putStrLn $ "status don't take no args, dude. Was given: " ++ Prelude.show args

add :: [String] -> IO ()
add filesToAdd = do
    stagingArea <- HIO.loadStagingArea
    HIO.writeStagingArea $ stagingArea { files = (files stagingArea) ++ filesToAdd }

rm :: [String] -> IO ()
rm args = do
    putStrLn $ "running command \"rm\" with args " ++ Prelude.show args

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
