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

import Data.ByteString as ByteString hiding (putStrLn, map)

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import qualified Database.LevelDB as DB

init :: [String] -> IO ()
init args = do
    rootDirAlreadyExists <- Dir.doesDirectoryExist Filesys.rootPath

    return $ map destructivelyCreateDirectory Filesys.directories

    -- return $ map (flip DB.open $ DB.defaultOptions{ DB.createIfMissing = True }) Filesys.databasePaths

    return $ map createFileWithContents Filesys.serializationPathsAndInitialContents

    currDir <- Dir.getCurrentDirectory
    if rootDirAlreadyExists
        then putStrLn
            $ "Re-initialized existing horse-control repository in"
            ++ currDir
        else putStrLn
            $ "Initialized existing horse-control repository in"
            ++ currDir

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

add :: [String] -> IO ()
add files = do
    putStrLn $ "running command \"add\" with args " ++ Prelude.show files

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

status :: [String] -> IO ()
status args = do
    putStrLn $ "running command \"status\" with args " ++ Prelude.show args

show :: [String] -> IO ()
show args = do
    putStrLn $ "running command \"show\" with args " ++ Prelude.show args