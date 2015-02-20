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

import qualified Horse.Constants as Constants

{-
~/.horseconfig : { name :: String
                 , email :: String }

./.horse
    info : { head :: Hash
           , stagingArea :: [diffhash] }

    diffs
        hash => (FilePath, diff)

    commits
        hash => ( author -- (name, e-mail)
                , date
                , hash
                , [parenthash]
                , [diffhash] ) -- TODO: tree?
-}


init :: [String] -> IO ()
init args = do
    -- root directory
    dirAlreadyExists <- Dir.doesDirectoryExist Constants.rootPath
    if dirAlreadyExists
        then Dir.removeDirectoryRecursive Constants.rootPath
        else return ()
    Dir.createDirectory Constants.rootPath

    -- diffs
    Dir.createDirectory Constants.diffsPath

    -- blobs
    Dir.createDirectory Constants.blobsPath

    -- info file
    infoHandle <- IO.openFile Constants.infoPath IO.WriteMode
    IO.hPutStr infoHandle Constants.infoInitialContents
    IO.hClose infoHandle

    currDir <- Dir.getCurrentDirectory
    if dirAlreadyExists
        then putStrLn
            $ "Re-initialized existing horse-control repository in"
            ++ currDir
        else putStrLn
            $ "Initialized existing horse-control repository in"
            ++ currDir

add :: [String] -> IO ()
add args = do
    putStrLn $ "running command \"add\" with args " ++ Prelude.show args

rm :: [String] -> IO ()
rm args = do
    putStrLn $ "running command \"rm\" with args " ++ Prelude.show args

checkout :: [String] -> IO ()
checkout args = do
    putStrLn $ "running command \"checkout\" with args " ++ Prelude.show args

commit :: [String] -> IO ()
commit args = do
    putStrLn $ "running command \"commit\" with args " ++ Prelude.show args

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
