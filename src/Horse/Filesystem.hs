-- | Helper module for manipulating the filesystem.
module Horse.Filesystem
( createFileWithContents
, destructivelyCreateDirectory
, getDirectoryContentsRecursiveSafe
, isPrefix
, dropPrefix
, relativizePath
, collapse
, repoRoot
, filesystemAncestors
, isRepositoryOrAncestorIsRepo
) where

import Control.Applicative
import Control.Monad

import Data.List ((\\))

import Filesystem.Path (parent)
import qualified Filesystem.Path (FilePath, collapse)
import Filesystem.Path.CurrentOS (decodeString, encodeString)

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.ByteString as ByteString

import Horse.Utils
    ( (</>)
    , iterateMaybe
    , toMaybe )
import qualified Horse.Constants as HC

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

-- | Returns whether the first parameter is a prefix of the second.
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] bs = True
isPrefix (a:as) [] = False
isPrefix (a:as) (b:bs)
    | a /= b = False
    | otherwise = isPrefix as bs

-- | assumes the first parameter is a prefix of the second; errors if
--   false
dropPrefix :: (Eq a) => [a] -> [a] -> [a]
dropPrefix [] bs = bs
dropPrefix (a:as) (b:bs)
    | a /= b = error "not a prefix"
    | otherwise = dropPrefix as bs

-- | Given (in order) a path to relativize, the root of the repo, and
--   the user's directory (to which the first parameter should be
--   relative), returns the first parameter made relative to the root
--   of the repo (the second parameter).
relativizePath :: FilePath -> FilePath -> IO FilePath
relativizePath path userDirectory = do
    root <- repoRoot
    return . collapse . tail $ (dropPrefix root userDirectory) </> path

repoRoot :: IO FilePath
repoRoot = do
    ancestors <- D.getCurrentDirectory >>= filesystemAncestors
    last <$> takeWhileM isRepositoryOrAncestorIsRepo ancestors
    where
        -- | Monadic 'takeWhile'.
        takeWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
        takeWhileM _ []     = return []
        takeWhileM p (x:xs) = do
            q <- p x
            if q
                then (takeWhileM p xs) >>= (return . (:) x)
                else return []

-- | Gets the ancestors of the current directory.
filesystemAncestors :: FilePath -> IO [FilePath]
filesystemAncestors directory = do
    let ancestors = iterateMaybe getParent (decodeString directory)
    return . (:) directory . map encodeString $ ancestors
    where
        getParent :: Filesystem.Path.FilePath -> Maybe Filesystem.Path.FilePath
        getParent curr = toMaybe (parent curr) ((/=) curr)

-- | Returns whether the current directory is part of a repository.
isRepositoryOrAncestorIsRepo :: FilePath -> IO Bool
isRepositoryOrAncestorIsRepo filepath
    = (filesystemAncestors filepath) >>= (fmap or . mapM isRepo)
    where
        -- | Returns whether the specified directory is part of a repository.
        isRepo :: FilePath -> IO Bool
        isRepo = D.doesDirectoryExist . (flip (</>) $ HC.repositoryDataDir)

-- | (wrapper around 'Filesystem.Path.collapse')
collapse :: FilePath -> FilePath
collapse
    = (\p -> if p == "" then "." else p)
    . encodeString
    . Filesystem.Path.collapse
    . decodeString
