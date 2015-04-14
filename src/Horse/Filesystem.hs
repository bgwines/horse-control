-- | Helper module for writing and reading data from disk.
module Horse.Filesystem
( createFileWithContents
, destructivelyCreateDirectory
) where

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.ByteString as ByteString

-- | Creates a file on disk with the specified content.
createFileWithContents :: FilePath -> ByteString.ByteString -> IO ()
createFileWithContents path contents = do
    handle <- IO.openFile path IO.WriteMode
    ByteString.hPutStr handle contents
    IO.hClose handle

-- | Creates a directory on disk at the specified destination, 
--   destroying one if it was already there.
destructivelyCreateDirectory :: FilePath -> IO ()
destructivelyCreateDirectory dir = do
    dirAlreadyExists <- D.doesDirectoryExist dir
    if dirAlreadyExists
        then D.removeDirectoryRecursive dir
        else return ()
    D.createDirectory dir
