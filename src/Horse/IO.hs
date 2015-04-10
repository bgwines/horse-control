{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module for writing and reading from disk / the database
module Horse.IO
( -- * staging area
  loadStagingArea
, writeStagingArea
, loadUnstagedFiles

-- * HEAD
, loadHeadHash
, writeHeadHash

-- * commits
, loadCommit
, writeCommit

-- * config
, loadConfig
, writeConfig

-- * paths
, repositoryDataDir
, headHashPath
, stagingAreaPath
, diffsPath
, commitsPath
, getConfigPath

-- * lists
, directories
, databasePaths
, serializationPathsAndInitialContents
, commitsHaveBeenMade

-- * utility functions
, createFileWithContents
, destructivelyCreateDirectory

-- * assorted
, loadHistory
, loadParent
, isRepositoryOrAncestorIsRepo
, refToHash

-- * diffing
, diffWithHEAD
, getStagedDiff
, applyDiff
) where

-- imports

import Prelude hiding (init, log, null)

import Data.Maybe

import Data.Monoid
import Control.Monad
import Control.Monad.Trans.Either

-- qualified imports

import qualified Filediff as FD
import qualified Filediff.Types as FD

import qualified Shelly as Sh

import qualified Data.Text as Text

import qualified System.IO as IO
import qualified System.Directory as Dir

import qualified Filesystem.Path (FilePath)

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

import qualified Data.Text.Punycode as Punycode (encode)

-- imported functions

import Data.List (foldl')

import Data.Maybe (isJust, fromJust)

import Data.Time.Clock (getCurrentTime, utctDay)

import Filesystem.Path (parent, null)
import Filesystem.Path.CurrentOS (decodeString, encodeString)

import Control.Applicative
import Control.Monad
import "monad-extras" Control.Monad.Extra (iterateMaybeM)
import Control.Monad.IO.Class (liftIO, MonadIO(..))

-- horse-control imports

import Horse.Types
import Horse.Utils
    ( maybeToEither
    , eitherToMaybe
    , stringToHash
    , putStrLn'
    , iterateMaybe
    , toMaybe
    , (</>))

-- | Writes a serializable object to a file
writeToFile :: (Serialize.Serialize a) => FilePath -> a -> IO ()
writeToFile filepath
    = ByteString.writeFile filepath
    . Serialize.encode

-- | Loads a serializable object from a file
loadFromFile :: (Serialize.Serialize a) => FilePath -> EitherT Error IO a
loadFromFile filepath = EitherT $ Serialize.decode <$> ByteString.readFile filepath

-- * staging area

-- | Loads the staging area from disk
loadStagingArea :: EitherT Error IO StagingArea
loadStagingArea = loadFromFile stagingAreaPath

-- | Writes the staging area to disk
writeStagingArea :: StagingArea -> IO ()
writeStagingArea = writeToFile stagingAreaPath

loadUnstagedFiles :: EitherT Error IO [FilePath]
loadUnstagedFiles = do
    allFilesDiff <- diffWithHEAD Nothing

    stagedFiles <- files <$> loadStagingArea

    right
        $ filter (not . (flip elem $ stagedFiles))
        . map FD.comp
        . FD.filediffs
        $ allFilesDiff

-- * HEAD

-- | Loads the object representing HEAD from disk
loadHeadHash :: EitherT Error IO Hash
loadHeadHash = loadFromFile headHashPath

-- | Writes the object representing HEAD to disk
writeHeadHash :: Hash -> IO ()
writeHeadHash = writeToFile headHashPath

-- * commits

-- | Loads the commit with the given hash from the database
loadCommit :: Hash -> EitherT Error IO Commit
loadCommit key = do
    maybeCommit <- liftIO $ do
        db <- DB.open commitsPath Default.def
        maybeCommit <- DB.get db Default.def key
        DBInternal.unsafeClose db
        return maybeCommit
    commit <- hoistEither $ maybeToEither loadErrorMessage maybeCommit
    hoistEither $ Serialize.decode commit
    where
        loadErrorMessage :: String
        loadErrorMessage = "Could not fetch commit for key " ++ (show key)

-- | Writes the commit with the given hash to the database
writeCommit :: Commit -> Hash -> IO ()
writeCommit commit key = do
    db <- DB.open commitsPath Default.def
    DB.put db Default.def key (Serialize.encode commit)
    DBInternal.unsafeClose db

-- * config

-- | Loads the object representing user-specified configuration from disk
loadConfig :: EitherT Error IO Config
loadConfig = (liftIO getConfigPath) >>= loadFromFile

-- | Writes the object representing user-specified configuration to disk
writeConfig :: Config -> IO ()
writeConfig config = do
    configPath <- getConfigPath
    ByteString.writeFile configPath (Serialize.encode config)

-- * paths

-- | The root path for all (horse-config)-stored data
repositoryDataDir :: FilePath
repositoryDataDir = ".horse"

-- | The path to where the object representing HEAD is stored
headHashPath :: FilePath
headHashPath = repositoryDataDir </> "HEAD"

-- | The path to where the object representing the staging area is stored
stagingAreaPath :: FilePath
stagingAreaPath = repositoryDataDir </> "stagingArea"

-- | The path to where diffs are stored
diffsPath :: FilePath
diffsPath = repositoryDataDir </> "diffs"

-- | The path to where commits are stored
commitsPath :: FilePath
commitsPath = repositoryDataDir </> "commits"

-- | The path to where the object representing user-specified
-- | configuration information is stored. Returnvalue is wrapped in
-- | the `IO` monad because getting the user's home directory is
-- | a monadic operation.
getConfigPath :: IO FilePath
getConfigPath = (++) <$> Dir.getHomeDirectory <*> (return "/.horseconfig")

-- * lists

-- | Paths to directories created by the implementation
directories :: [FilePath]
directories = [repositoryDataDir, diffsPath, commitsPath]

-- | Paths to databases used in the implementation
databasePaths :: [FilePath]
databasePaths = [diffsPath, commitsPath]

-- | Paths to files used for storing objects, and the initial contents of
-- | those files upon initialization of an empty repository
serializationPathsAndInitialContents :: [(FilePath, ByteString.ByteString)]
serializationPathsAndInitialContents =
    [ (headHashPath, Serialize.encode $ (Default.def :: Hash))
    , (stagingAreaPath, Serialize.encode $ (Default.def :: StagingArea)) ]

-- * utility functions

-- | Creates a file on disk with the specified content
createFileWithContents :: (FilePath, ByteString.ByteString) -> IO ()
createFileWithContents (path, contents) = do
    handle <- IO.openFile path IO.WriteMode
    ByteString.hPutStr handle contents
    IO.hClose handle

-- | Creates a directory on disk at the specified destination, 
-- | destroying one if it was already there
destructivelyCreateDirectory :: FilePath -> IO ()
destructivelyCreateDirectory dir = do
    dirAlreadyExists <- Dir.doesDirectoryExist dir
    if dirAlreadyExists
        then Dir.removeDirectoryRecursive dir
        else return ()
    Dir.createDirectory dir

-- * assorted

-- | Loads the history from a given commit, all the way back
-- | to the start. Returns in reverse order (latest commit at front)
loadHistory :: Commit -> EitherT Error IO [Commit]
loadHistory commit
    = liftIO
    . fmap ((:) commit)
    $ iterateMaybeM
        (fmap eitherToMaybe . runEitherT . loadParent)
        commit

-- | Attempts to load the parent commit for a given commit
loadParent :: Commit -> EitherT Error IO Commit
loadParent commit =
    if isJust $ parentHash commit
        then loadCommit
            . fromJust
            . parentHash
            $ commit
        else left $ "No parent for commit with hash " ++ (show . hash $ commit)

-- | Given any string, attempt to convert it to a hash.
-- | Succeeds if the string is in a hash format, even if
-- | the hash is not a key in the database (no commits / diffs
-- | have been hashed with that key yet). Fails if the format
-- | is unexpected. Acceptable formats are listed in user-facing
-- | documentation
refToHash :: String -> EitherT Error IO Hash
refToHash unparsedRef = do
    base <- case baseRef of
        "HEAD" -> loadHeadHash -- TODO: constant?
        someHash -> return $ stringToHash someHash
    final <- (hoistEither relatives) >>= applyRelatives base
    putStrLn' . show $ final
    right final
    where
        parentSyntax :: Char
        parentSyntax = '^'

        applyRelatives :: Hash -> [Relative] -> EitherT Error IO Hash
        applyRelatives h [] = right h

        baseRef :: String
        baseRef = takeWhile (not . isRelativeSyntax) unparsedRef

        relatives :: Either Error [Relative]
        relatives = mapM toRelative $ dropWhile (not . isRelativeSyntax) unparsedRef

        toRelative :: Char -> Either Error Relative
        toRelative ch = if ch == parentSyntax
            then Right Parent
            else Left "Undefined relative syntax"

        isRelativeSyntax :: Char -> Bool
        isRelativeSyntax ch = ch == parentSyntax

-- | Returns whether the specified directory is part of a repository.
isRepo :: FilePath -> IO Bool
isRepo = Dir.doesDirectoryExist . (flip (</>) $ repositoryDataDir)

-- | Gets the ancestors of the current directory
filesystemAncestors :: IO [FilePath]
filesystemAncestors = do
    currentDirectory <- decodeString <$> Dir.getCurrentDirectory
    let ancestors = iterateMaybe getParent currentDirectory
    return . map encodeString . (:) currentDirectory $ ancestors
    where
        getParent :: Filesystem.Path.FilePath -> Maybe Filesystem.Path.FilePath
        getParent curr = toMaybe (parent curr) ((/=) curr)

-- | Returns whether the current directory is part of a repository.
isRepositoryOrAncestorIsRepo :: IO Bool
isRepositoryOrAncestorIsRepo
    = filesystemAncestors >>= (fmap or . mapM isRepo)

-- | Identifies whether any commits have been made in the current repository
commitsHaveBeenMade :: EitherT Error IO Bool
commitsHaveBeenMade = ((/=) Default.def) <$> loadHeadHash

-- * diffing

-- | Pass in `Nothing` to diff all files; otherwise, pass in
-- the files to diff
diffWithHEAD :: Maybe [FilePath] -> EitherT Error IO FD.Diff
diffWithHEAD maybeFilesToDiff = do
    headExists <- commitsHaveBeenMade
    diffWithRoot <- if headExists
        then do
            headHash <- loadHeadHash
            commit <- loadCommit headHash

            diffs <- (reverse . map diffWithPrimaryParent) <$> loadHistory commit
            let diffWithRoot = foldl' mappend mempty diffs
            return diffWithRoot
        else return mempty

    -- TODO: no EitherT here?
    headDir <- liftIO $ ((</>) repositoryDataDir) <$> getTempDirectory
    liftIO $ Dir.createDirectory headDir
    liftIO $ FD.applyToDirectory diffWithRoot headDir

    allFilesDiff <- liftIO $ FD.diffDirectoriesWithIgnoredSubdirs headDir "." [] [headDir, repositoryDataDir]

    liftIO $ Dir.removeDirectoryRecursive headDir

    if isNothing maybeFilesToDiff
        then right allFilesDiff
        else do
            let filesToDiff = fromMaybe undefined maybeFilesToDiff
            -- `comp` instead of `base` because `comp` is ".", whereas
            -- `base` is ".horse/<temp directory>"
            right
                $ FD.Diff
                . filter ((flip elem $ filesToDiff) . FD.comp)
                . FD.filediffs
                $ allFilesDiff
    where
        -- | Gives a name of a directory that is pretty much
        -- guaranteed to exist, so it's free for creation.
        getTempDirectory :: IO FilePath
        getTempDirectory = (map formatChar . show) <$> getCurrentTime

        -- | Some characters can't be in directory names.
        formatChar :: Char -> Char
        formatChar ' ' = '-'
        formatChar '.' = '-'
        formatChar ':' = '-'
        formatChar ch = ch

-- | Get a diff between the staging area and HEAD
getStagedDiff :: StagingArea -> EitherT Error IO FD.Diff
getStagedDiff = diffWithHEAD . Just . files

-- | True upon success, False upon failure
applyDiff :: FD.Diff -> EitherT Error IO ()
applyDiff = liftIO . (flip FD.applyToDirectory) "."
