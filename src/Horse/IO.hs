{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Helper module for writing and reading data from disk.
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

-- * assorted
, commitsHaveBeenMade
, checkoutToDirectory
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
import qualified System.Directory as D

import qualified Filesystem.Path (FilePath)

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

import qualified Data.Text.Punycode as Punycode (encode)

-- imported functions

import Data.List (foldl', (\\))

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
    , (</>)
    , whenM )
import qualified Horse.Constants as HC

-- | Writes a serializable object to a file.
writeToFile :: (Serialize.Serialize a) => FilePath -> a -> IO ()
writeToFile filepath
    = ByteString.writeFile filepath
    . Serialize.encode

-- | Loads a serializable object from a file.
loadFromFile :: (Serialize.Serialize a) => FilePath -> EitherT Error IO a
loadFromFile filepath = EitherT $ Serialize.decode <$> ByteString.readFile filepath

-- * staging area

-- | Loads the staging area from disk.
loadStagingArea :: EitherT Error IO StagingArea
loadStagingArea = loadFromFile HC.stagingAreaPath

-- | Writes the staging area to disk.
writeStagingArea :: StagingArea -> IO ()
writeStagingArea = writeToFile HC.stagingAreaPath

-- | Gets all files that have modifications that are not staged.
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

-- | Loads hash of HEAD from disk. Returns a `Left` if no commits have
--   been made.
loadHeadHash :: EitherT Error IO Hash
loadHeadHash = loadFromFile HC.headHashPath

-- | Writes HEAD's hash to disk.
writeHeadHash :: Hash -> IO ()
writeHeadHash = writeToFile HC.headHashPath

-- * commits

-- | Attempts to load the commit with the given hash from the database.
--   Returns a `Left` if the specified `Hash` doesn't reference a commit
--   or if deserialization fails (which might happen if the database
--   got corrupted).
loadCommit :: Hash -> EitherT Error IO Commit
loadCommit key = do
    maybeCommit <- liftIO $ do
        db <- DB.open HC.commitsPath Default.def
        maybeCommit <- DB.get db Default.def key
        DBInternal.unsafeClose db
        return maybeCommit
    commit <- hoistEither $ maybeToEither loadErrorMessage maybeCommit
    hoistEither $ Serialize.decode commit
    where
        loadErrorMessage :: String
        loadErrorMessage = "Could not fetch commit for key " ++ (show key)

-- | Writes the commit to the database, under the key of its hash.
writeCommit :: Commit -> IO ()
writeCommit commit = do
    db <- DB.open HC.commitsPath Default.def
    DB.put db Default.def (hash commit) (Serialize.encode commit)
    DBInternal.unsafeClose db

-- * config

-- | Loads the object representing user-specified configuration from disk.
--   Returns a `Left` if the specified `Hash` doesn't reference a commit
--   or if deserialization fails (which might happen if the database
--   got corrupted).
loadConfig :: EitherT Error IO Config
loadConfig = (liftIO HC.configPath) >>= loadFromFile

-- | Writes the object representing user-specified configuration to disk.
writeConfig :: Config -> IO ()
writeConfig config = do
    configPath <- HC.configPath
    ByteString.writeFile configPath (Serialize.encode config)

-- * assorted

-- | Identifies whether any commits have been made in the current
--   repository.
commitsHaveBeenMade :: EitherT Error IO Bool
commitsHaveBeenMade = ((/=) Default.def) <$> loadHeadHash


-- | Checks out the specified hash to the specified directory. *NOTE*:
--   will entirely overwrite the contents of the specified directory;
--   be careful.
checkoutToDirectory :: FilePath -> Hash -> EitherT Error IO ()
checkoutToDirectory dir hash = do
    liftIO clearDirectory
    history <- loadCommit hash >>= loadHistory
    let diffs = reverse . map diffWithPrimaryParent $ history
    let diffWithRoot = foldl' mappend mempty diffs

    liftIO $ FD.applyToDirectory diffWithRoot dir
    where
        clearDirectory :: IO ()
        clearDirectory = do
            allContents <- liftIO $ D.getDirectoryContents dir
            let toDelete = allContents \\ [HC.repositoryDataDir, "..", "."]
            mapM_ rm toDelete

        rm :: FilePath -> IO ()
        rm path = do
            isFile <- D.doesFileExist path
            if isFile
                then D.removeFile path
                else D.removeDirectoryRecursive path

-- | Loads the history from a given commit, all the way back
--   to the start. Returns in reverse order (latest commit at front).
loadHistory :: Commit -> EitherT Error IO [Commit]
loadHistory commit
    = liftIO
    . fmap ((:) commit)
    $ iterateMaybeM
        (fmap eitherToMaybe . runEitherT . loadParent)
        commit

-- | Attempts to load the parent commit for a given commit.
loadParent :: Commit -> EitherT Error IO Commit
loadParent commit =
    if isJust $ parentHash commit
        then loadCommit
            . fromJust
            . parentHash
            $ commit
        else left $ "No parent for commit with hash " ++ (show . hash $ commit)

-- | Given any string, attempt to convert it to a hash.
--   Succeeds if the string is in a hash format, even if
--   the hash is not a key in the database (no commits / diffs
--   have been hashed with that key yet). Fails if the format
--   is unexpected. Acceptable formats are listed in user-facing
--   documentation.
refToHash :: String -> EitherT Error IO Hash
refToHash unparsedRef = do
    base <- case baseRef of
        "HEAD" -> loadHeadHash
        someHash -> return $ stringToHash someHash
    final <- (hoistEither relatives) >>= applyRelatives base
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
isRepo = D.doesDirectoryExist . (flip (</>) $ HC.repositoryDataDir)

-- | Gets the ancestors of the current directory.
filesystemAncestors :: IO [FilePath]
filesystemAncestors = do
    currentDirectory <- decodeString <$> D.getCurrentDirectory
    let ancestors = iterateMaybe getParent currentDirectory
    return . map encodeString . (:) currentDirectory $ ancestors
    where
        getParent :: Filesystem.Path.FilePath -> Maybe Filesystem.Path.FilePath
        getParent curr = toMaybe (parent curr) ((/=) curr)

-- | Returns whether the current directory is part of a repository.
isRepositoryOrAncestorIsRepo :: IO Bool
isRepositoryOrAncestorIsRepo
    = filesystemAncestors >>= (fmap or . mapM isRepo)

-- * diffing

-- | Pass in `Nothing` to diff all files; otherwise, pass in
--   the files to diff.
diffWithHEAD :: Maybe [FilePath] -> EitherT Error IO FD.Diff
diffWithHEAD maybeFilesToDiff = do
    headDir <- liftIO $ ((</>) HC.repositoryDataDir) <$> getTempDirectory
    liftIO $ D.createDirectory headDir

    -- if no commits have been made; no point in filling the directory
    -- since HEAD doesn't exist so it would result in an empty diff
    -- anyway.
    whenM commitsHaveBeenMade $ do
        loadHeadHash >>= checkoutToDirectory headDir

    allFilesDiff <- liftIO $ FD.diffDirectoriesWithIgnoredSubdirs headDir "." [] [HC.repositoryDataDir]

    liftIO $ D.removeDirectoryRecursive headDir

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

-- | Get a diff of the changes stored in the staging aread (that is,
--   the changes to HEAD that will be committed).
getStagedDiff :: StagingArea -> EitherT Error IO FD.Diff
getStagedDiff = diffWithHEAD . Just . files

-- | `True` upon success, `False` upon failure.
applyDiff :: FD.Diff -> EitherT Error IO ()
applyDiff = liftIO . (flip FD.applyToDirectory) "."
