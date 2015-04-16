-- | Helper module for manipulating the filesystem.
module Horse.Filesystem
( createFileWithContents
, destructivelyCreateDirectory
, getDirectoryContentsRecursiveSafe
) where

import Control.Applicative
import Control.Monad

import Data.List ((\\))

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.ByteString as ByteString

import Horse.Utils ((</>))

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

-- | Gets paths to all files in or in subdirectories of the
-- | specified directory. Returned paths are relative to the
-- | given directory.
getDirectoryContentsRecursiveSafe :: FilePath -> IO [FilePath]
getDirectoryContentsRecursiveSafe directory = do
    contents <- getDirectoryContentsRecursiveSafe' directory

    let directoryWithTrailingSlash = if last directory == '/'
        then directory
        else directory </> ""
    let numPathComponents = length . filter ((==) '/') $ directoryWithTrailingSlash
    let removePathComponents = last . take (numPathComponents + 1) . iterate removeFirstPathComponent

    return . map removePathComponents $ contents

getDirectoryContentsRecursiveSafe' :: FilePath -> IO [FilePath]
getDirectoryContentsRecursiveSafe' directory = do
    exists <- D.doesDirectoryExist directory
    if not exists
        then return []
        else do
            relativeContents <- removeDotDirs <$> D.getDirectoryContents directory
            let contents = map ((</>) directory) relativeContents

            files <- filterM D.doesFileExist contents
            directories <- filterM D.doesDirectoryExist contents

            recFiles <- concat <$> mapM getDirectoryContentsRecursiveSafe' directories

            return $ files ++ recFiles

-- | Removes the oldest ancestor from a path component, e.g.
-- |
-- |     > removeFirstPathComponent "a/b/c"
-- |     "b/c"
removeFirstPathComponent :: FilePath -> FilePath
removeFirstPathComponent path =
    if null . filter ((==) '/') $ path
         then error "path without '/' in it"
         else tail . dropUntil ((==) '/') $ path

-- | Takes a list of filepaths, and removes "." and ".." from it.
removeDotDirs :: [FilePath] -> [FilePath]
removeDotDirs = flip (\\) $ [".", ".."]

-- | Drops elements from the given list until the predicate function
-- | returns `True` (returned list includes element that passes test)
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil f (x:xs) =
    if f x
        then (x:xs)
        else dropUntil f xs
