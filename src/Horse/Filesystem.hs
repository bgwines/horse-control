-- | Helper module for manipulating the filesystem.
module Horse.Filesystem
( createFileWithContents
, destructivelyCreateDirectory
, getDirectoryContentsRecursiveSafe
, dropPrefix
, dropUntil
, takeWhileM
, relativizePath
, collapse
, repoRoot
, filesystemAncestors
, isInRepository
, assertIsRepositoryAndCdToRoot
, assertCurrDirIsRepo
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Control.Exception.Base (assert)

import Control.Conditional (whenM)

import Data.Maybe
import Data.List ((\\))

import Filesystem.Path (parent)
import qualified Filesystem.Path (FilePath, collapse)
import Filesystem.Path.CurrentOS (decodeString, encodeString)

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.ByteString as ByteString

import Horse.Types
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
    whenM (D.doesDirectoryExist dir) $
        D.removeDirectoryRecursive dir
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
    let numPathComponents = length . filter ('/' ==) $ directoryWithTrailingSlash
    let removePathComponents = last . take (numPathComponents + 1) . iterate removeFirstPathComponent

    return . map removePathComponents $ contents

getDirectoryContentsRecursiveSafe' :: FilePath -> IO [FilePath]
getDirectoryContentsRecursiveSafe' directory = do
    exists <- D.doesDirectoryExist directory
    if not exists
        then return []
        else do
            relativeContents <- removeDotDirs <$> D.getDirectoryContents directory
            let contents = map (directory </>) relativeContents

            files <- filterM D.doesFileExist contents
            directories <- filterM D.doesDirectoryExist contents

            recFiles <- concat <$> mapM getDirectoryContentsRecursiveSafe' directories

            return $ files ++ recFiles

-- | Removes the oldest ancestor from a path component, e.g.
--  
--       > removeFirstPathComponent "a/b/c"
--       "b/c"
--
-- Errors if given 'FilePath' doesn't have any \'/\' in it.
removeFirstPathComponent :: FilePath -> FilePath
removeFirstPathComponent = dropUntil ('/' ==)

-- | Takes a list of filepaths, and removes "." and ".." from it.
removeDotDirs :: [FilePath] -> [FilePath]
removeDotDirs = flip (\\) [".", ".."]

-- | Drops elements from the given list until the predicate function
--   returns `True`. Note: API is such that the returned list does NOT
--   include the element that passes test)
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil f (x:xs) =
    if f x
        then xs
        else dropUntil f xs

-- | Assumes the first parameter is a prefix of the second -- if not,
--   it'll just drop all of the first one anyway.
dropPrefix :: (Eq a) => [a] -> [a] -> Maybe [a]
dropPrefix [] bs = Just bs
dropPrefix _  [] = Nothing
dropPrefix (a:as) (b:bs) =
    if a == b
        then dropPrefix as bs
        else Nothing

-- | Given a path to relativize and the user's directory (to which the
--   first parameter should be relative), returns the first parameter
--   made relative to the root of the repo.
relativizePath :: FilePath -> FilePath -> EIO FilePath
relativizePath path userDirectory = do
    root <- repoRoot
    let userDirectoryRelativeToRoot = dropPrefix root userDirectory
    right . collapse . tail $ fromJust userDirectoryRelativeToRoot </> path

-- | If the current directory is a repo or a subdirectory of one,
--   gets the ancestor that is a repo. If none are, returns an error
--   state.
repoRoot :: EIO FilePath
repoRoot = do
    ancestors <- liftIO $ D.getCurrentDirectory >>= filesystemAncestors
    repoAncestors <- liftIO $ takeWhileM isInRepository ancestors
    if null repoAncestors
        then left "Fatal: current directory is not a repo or a decendant of one."
        else right . last $ repoAncestors

-- | Monadic 'takeWhile'.
takeWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ []     = return []
takeWhileM p (x:xs) = do
    q <- p x
    if q
        then liftM ((:) x) (takeWhileM p xs)
        else return []

-- | Gets the ancestors of the current directory.
filesystemAncestors :: FilePath -> IO [FilePath]
filesystemAncestors directory = do
    let ancestors = iterateMaybe getParent (decodeString directory)
    return . (:) directory . map encodeString $ ancestors
    where
        getParent :: Filesystem.Path.FilePath -> Maybe Filesystem.Path.FilePath
        getParent curr = toMaybe (parent curr) (curr /=)

-- | Returns whether the current directory is part of a repository.
isInRepository :: FilePath -> IO Bool
isInRepository filepath
    = D.canonicalizePath filepath
    >>= filesystemAncestors
    >>= (fmap or . mapM isRepo)

-- | Returns whether the specified directory is part of a repository.
isRepo :: FilePath -> IO Bool
isRepo = D.doesDirectoryExist . (</> HC.repositoryDataDir)

-- | (wrapper around 'Filesystem.Path.collapse')
collapse :: FilePath -> FilePath
collapse
    = (\p -> if p == "" then "." else p)
    . encodeString
    . Filesystem.Path.collapse
    . decodeString

assertIsRepositoryAndCdToRoot :: EIO ()
assertIsRepositoryAndCdToRoot = do
    userDirectory <- liftIO D.getCurrentDirectory
    isRepository <- liftIO $ isInRepository userDirectory
    unless isRepository $
        left "Fatal: Not a horse repository (or any of the ancestor directories)."
    repoRoot >>= liftIO . D.setCurrentDirectory

-- | Used to assert implementation invariants
assertCurrDirIsRepo :: IO ()
assertCurrDirIsRepo = assertM (isRepo ".") ()
    where
        assertM :: IO Bool -> a -> IO a
        assertM bool x = liftM (`assert` x) bool
