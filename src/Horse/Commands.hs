{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Commands exposed through the CLI
module Horse.Commands
( -- * Basic commands
  Horse.Commands.config
, Horse.Commands.init
, Horse.Commands.status
, Horse.Commands.stage
, Horse.Commands.commit
, Horse.Commands.commitAmend
, Horse.Commands.checkout
, Horse.Commands.show
, Horse.Commands.log
, Horse.Commands.squash
) where

-- imports

import Prelude hiding (init, log, show)
import qualified Prelude (show)

import GHC.Generics

import Data.Monoid
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either

-- qualified imports

import qualified Zora.List as ZL (take_while_keep_last)

import qualified Filediff as FD
import qualified Filediff.Stats as FD
import qualified Filediff.Types as FD

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.Hex as Hex
import qualified Data.Default as Default
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString

import qualified Crypto.Hash.SHA256 as SHA256

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBI

-- imported functions

import qualified Filesystem.Path (FilePath, collapse)

import Data.List (foldl', (\\), isInfixOf, isPrefixOf)

import Data.Maybe (fromMaybe, isNothing, isJust, fromJust, catMaybes)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Filesystem.Path (parent, null)
import Filesystem.Path.CurrentOS (decodeString, encodeString)

import "monad-extras" Control.Monad.Extra (iterateMaybeM)

-- horse-control imports

import Horse.Types
import Horse.Utils
    ( stringToHash
    , hashToString
    , (|<$>|)
    , print'
    , putStrLn'
    , eitherToMaybe
    , fromEitherMaybeDefault
    , (</>)
    , whenM
    , unlessM
    , toMaybe )
import qualified Horse.IO as HIO
import qualified Horse.Filesystem as HF
import qualified Horse.Constants as HC

-- | Sets user-specific configuration information. The `Maybe String`
--   refers to the user's name.
config :: Maybe String -> Maybe EmailAddress -> IO ()
config maybeName maybeEmail = do
    configPath <- HC.configPath
    configFileExistedBefore <- D.doesFileExist configPath

    if configFileExistedBefore
        then do
            eitherUserInfo <- runEitherT $ userInfo <$> HIO.loadConfig
            let updatedUserInfo = UserInfo {
                  name = fromEitherMaybeDefault
                    (name <$> eitherUserInfo)
                    maybeName
                , email = fromEitherMaybeDefault
                    (email <$> eitherUserInfo)
                    maybeEmail }
            HIO.writeConfig $ Config { userInfo = updatedUserInfo }
        else do
            HF.createFileWithContents configPath ByteString.empty

            let userInfo = UserInfo {
                  name = fromMaybe Default.def maybeName
                , email = fromMaybe Default.def maybeEmail }
            HIO.writeConfig $ Config { userInfo = userInfo }

-- | Initializes an empty repository in the current directory. If
--   one currently exists, it aborts.
init :: Maybe Verbosity -> EitherT Error IO ()
init maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    whenM (liftIO $ HF.isRepositoryOrAncestorIsRepo ".") $
        left "Error: directory is or is subdirectory of another horse-control repo"

    liftIO $ do
        -- initialize config file; it's read from
        -- in this function and hence needs to exist
        config Nothing Nothing 

        mapM_ HF.destructivelyCreateDirectory HC.directories

        let createOptions = DB.defaultOptions{ DB.createIfMissing = True }
        mapM_ ((=<<) DBI.unsafeClose . (flip DB.open) createOptions)
            HC.databasePaths

        mapM_ (uncurry HF.createFileWithContents) HC.serializationPathsAndInitialContents

        currDir <- D.getCurrentDirectory
        unless (verbosity == Quiet) $
            putStrLn $ "Initialized existing horse-control repository in"
                ++ currDir ++ "/" ++ HC.repositoryDataDir

-- | Gets and prints the difference between the current state of the
-- filesystem and the state of the filesystem at HEAD.
status :: Maybe Verbosity -> EitherT Error IO Status
status maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    userDirectory <- liftIO D.getCurrentDirectory
    assertIsRepositoryAndCdToRoot

    stagingArea <- HIO.loadStagingArea
    unstagedFiles <- loadUnstagedFiles
    let currentStatus = Status stagingArea unstagedFiles

    unless (verbosity == Quiet) $ do
        putStrLn' "Staged changes:"
        print' stagingArea
        putStrLn' ""

        putStrLn' "Unstaged changes:"
        print' unstagedFiles

    liftIO $ D.setCurrentDirectory userDirectory
    right currentStatus
    where
        -- | Gets all files that have modifications that are not staged.
        loadUnstagedFiles :: EitherT Error IO [FilePath]
        loadUnstagedFiles = do
            stagedFiles <- files <$> HIO.loadStagingArea
            diffWithHEAD Nothing
                >>= right . getUnstagedFilesFromDiff stagedFiles

        getUnstagedFilesFromDiff :: [FilePath] -> FD.Diff -> [FilePath]
        getUnstagedFilesFromDiff stagedFiles
            = filter (not . (flip elem $ stagedFiles))
            . map FD.comp
            . FD.filediffs

-- | Adds the whatever change was made (modification or addition or
--   deletion) to the specified file or directory to the staging area.
stage :: String -> EitherT Error IO StagingArea
stage path = do
    -- can't yet tell which files were ever committed, so we can't
    -- distinguish between a deleted file and a nonexistent one.
    --pathExistsIsFile <- liftIO $ D.doesFileExist path
    --pathExistsIsDir <- liftIO $ D.doesDirectoryExist path
    --unless (pathExistsIsFile || pathExistsIsDir) $ do
    --    left $ "Can't stage file or directory at path \"" ++ path ++ "\"; no file or directory exists at that path."

    userDirectory <- liftIO D.getCurrentDirectory
    assertIsRepositoryAndCdToRoot

    -- tail for prefixing '/' coming from `dropPrefix`
    -- relative to root of repo
    relativePath <- liftIO $ HF.relativizePath path userDirectory
    when (isPrefixOf ".." relativePath) $ do
        left $ "Can't stage file or directory outside of the repository: " ++ path
    relativePaths <- getRelativePaths relativePath

    diffs <- FD.filediffs <$> (diffWithHEAD $ Just relativePaths)

    stagingArea <- HIO.loadStagingArea

    let updateFunctions = zipWith ($) (repeat updateStagingArea) diffs
    let updatedStagingArea = foldl (flip ($)) stagingArea updateFunctions

    liftIO $ do
        HIO.writeStagingArea updatedStagingArea
        D.setCurrentDirectory userDirectory

    right updatedStagingArea
    where
        getRelativePaths :: FilePath -> EitherT Error IO [FilePath]
        getRelativePaths relativePath = do
            isDir <- liftIO $ D.doesDirectoryExist relativePath

            unfiltered <- if isDir
                then liftIO $ (map (HF.collapse . (</>) relativePath)) <$> HF.getDirectoryContentsRecursiveSafe relativePath
                else right [relativePath]

            let filtered = filter (not . isInfixOf HC.repositoryDataDir) unfiltered

            right $ map (\p -> if "./" `isPrefixOf` p then drop 2 p else p) filtered

        updateStagingArea :: FD.Filediff -> StagingArea -> StagingArea
        updateStagingArea (FD.Filediff base _ change) stagingArea =
            case change of {
            FD.Add _ -> stagingArea { adds = base : (adds stagingArea) };
            FD.Mod _ -> stagingArea { mods = base : (mods stagingArea) };
            FD.Del _ -> stagingArea { dels = base : (dels stagingArea) };
            }

-- | Writes the changes housed in the staging area as a commit to disk,
--   then clears the staging area.
commitAmend :: Maybe String -> Maybe Verbosity -> EitherT Error IO Commit
commitAmend maybeMessage maybeVerbosity = do
    userDirectory <- liftIO D.getCurrentDirectory
    assertIsRepositoryAndCdToRoot

    unlessM commitsHaveBeenMade $
        left "Error: cannot amend when no commits have been made."

    latestCommit <- commit maybeMessage maybeVerbosity
    squashedCommit <- squash (hashToString . fromJust . parentHash $ latestCommit)

    liftIO $ D.setCurrentDirectory userDirectory
    right squashedCommit

-- | Writes the changes housed in the staging area as a commit to disk,
--   then clears the staging area.
commit :: Maybe String -> Maybe Verbosity -> EitherT Error IO Commit
commit maybeMessage maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity
    let message = fromMaybe "default message" maybeMessage

    userDirectory <- liftIO D.getCurrentDirectory
    assertIsRepositoryAndCdToRoot

    -- commit params
    now <- liftIO $ fmap (toGregorian . utctDay) getCurrentTime

    isFirstCommit <- ((==) Default.def) <$> HIO.loadHeadHash
    parent <- if isFirstCommit
        then right Nothing
        else HIO.loadHeadHash >>= (fmap Just . HIO.loadCommit)

    stagedDiff <- HIO.loadStagingArea >>= diffWithHEAD . Just . files

    config <- HIO.loadConfig

    let hashlessCommit = Commit {
        author                  = userInfo config
        , date                  = now
        , hash                  = Default.def -- no hash yet since commit
                                              -- hasn't been created
        , parentHash            = hash <$> parent
        , diffWithPrimaryParent = stagedDiff
        , message               = message }

    let commitHash = hashCommit hashlessCommit
    let completeCommit = hashlessCommit { hash = commitHash }

    liftIO $ do
        HIO.writeCommit completeCommit
        HIO.writeHeadHash commitHash
        HIO.writeStagingArea (Default.def :: StagingArea)
        D.setCurrentDirectory userDirectory

    when (verbosity == Verbose) $ do
        print' completeCommit

    unless (verbosity == Quiet) $ do
        putStrLn' $ "[<branch> "
            ++ (Prelude.show . ByteString.take 8 $ commitHash)
            ++  "] " ++ message

        let numFiles = FD.numFilesAffected stagedDiff
        let numAdds = FD.numAddedLines stagedDiff
        let numDels = FD.numDeletedLines stagedDiff
        let filesString = if numFiles == 1
            then " file"
            else " files"
        putStrLn' $ ""
            ++ Prelude.show numFiles ++ filesString ++ " changed, "
            ++ Prelude.show numAdds ++ " insertions(+), "
            ++ Prelude.show numDels ++ " deletions(-)"

    right completeCommit

-- TODO: where does this go?
hashCommit :: Commit -> Hash
hashCommit
    = ByteString.take 40
    . Hex.hex
    . SHA256.hash
    . Serialize.encode

-- | Inclusive in param
squash :: String -> EitherT Error IO Commit
squash ref = do
    userDirectory <- liftIO D.getCurrentDirectory
    assertIsRepositoryAndCdToRoot

    historyToRoot <- log Nothing Nothing (Just Quiet)

    endHash <- refToHash ref
    let squashedCommit = getSquashedCommit historyToRoot endHash

    liftIO $ do
        HIO.writeCommit squashedCommit
        HIO.writeHeadHash (hash squashedCommit)
        D.setCurrentDirectory userDirectory

    right squashedCommit
    where
        getSquashedCommit :: [Commit] -> Hash -> Commit
        getSquashedCommit historyToRoot endHash =
            unhashedCommit { hash = hashCommit unhashedCommit }
            where
                history :: [Commit]
                history = ZL.take_while_keep_last ((/=) endHash . hash) historyToRoot

                squashBase :: Commit
                squashBase = last history

                squashedDiff :: FD.Diff
                squashedDiff = mconcat . map diffWithPrimaryParent . reverse $ history

                unhashedCommit :: Commit
                unhashedCommit = Commit {
                    author                  = author squashBase
                    , date                  = date squashBase
                    , hash                  = Default.def
                    , parentHash            = parentHash squashBase
                    , diffWithPrimaryParent = squashedDiff
                    , message               = mconcat . map message $ history }

-- | Sets the contents of the filesystem to the state it had in the
--   specified commit.
checkout :: String -> Maybe Verbosity -> EitherT Error IO ()
checkout ref maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    userDirectory <- liftIO D.getCurrentDirectory
    assertIsRepositoryAndCdToRoot

    refToHash ref >>= checkoutToDirectory "."

    liftIO $ D.setCurrentDirectory userDirectory

-- | Prints information about the specified commit to the console. With
--   a `Nothing` for its parameter, it assumes a single argument of HEAD.
show :: Maybe String -> Maybe Verbosity -> EitherT Error IO Commit
show maybeRef maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    userDirectory <- liftIO D.getCurrentDirectory
    assertIsRepositoryAndCdToRoot

    ref <- fromMaybe HIO.loadHeadHash (refToHash <$> maybeRef)
    commit <- HIO.loadCommit ref

    when (verbosity /= Quiet) $ print' commit

    liftIO $ D.setCurrentDirectory userDirectory

    right commit

-- | Prints the history from the current commit backwards. With
--   a `Nothing` for its parameter, it assumes a single argument of HEAD.
--   Pass in a `Just` `Int` to specify the number of commits back to go
--   in the history.
log :: Maybe String -> Maybe Int -> Maybe Verbosity -> EitherT Error IO [Commit]
log maybeRef maybeNumCommits maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    userDirectory <- liftIO D.getCurrentDirectory
    assertIsRepositoryAndCdToRoot

    haveCommitsBeenMade <- commitsHaveBeenMade
    history <- if haveCommitsBeenMade
        then do
            commitHash <- maybe HIO.loadHeadHash refToHash maybeRef
            commit <- HIO.loadCommit commitHash
            history <- (take <$> maybeNumCommits) |<$>| loadHistory commit

            unless (verbosity == Quiet) $ do
                print' $ hash <$> history
            right history
        else right []

    liftIO $ D.setCurrentDirectory userDirectory

    right history

-- * helper functions (not exposed)

-- * assorted

assertIsRepositoryAndCdToRoot :: EitherT Error IO ()
assertIsRepositoryAndCdToRoot = do
    userDirectory <- liftIO D.getCurrentDirectory
    isRepository <- liftIO $ HF.isRepositoryOrAncestorIsRepo userDirectory
    unless isRepository $ do
        left $ "Fatal: Not a horse repository (or any of the ancestor directories)."
    liftIO $ HF.repoRoot >>= D.setCurrentDirectory

-- | Identifies whether any commits have been made in the current
--   repository.
commitsHaveBeenMade :: EitherT Error IO Bool
commitsHaveBeenMade = ((/=) Default.def) <$> HIO.loadHeadHash

-- | Checks out the specified hash to the specified directory. *NOTE*:
--   will entirely overwrite the contents of the specified directory;
--   be careful.
checkoutToDirectory :: FilePath -> Hash -> EitherT Error IO ()
checkoutToDirectory dir hash = do
    liftIO clearDirectory
    history <- HIO.loadCommit hash >>= loadHistory
    let diffs = reverse . map diffWithPrimaryParent $ history
    let diffWithRoot = foldl' mappend mempty diffs

    liftIO $ FD.applyToDirectory diffWithRoot dir
    where
        clearDirectory :: IO ()
        clearDirectory = do
            allContents <- D.getDirectoryContents dir
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
    . iterateMaybeM (fmap eitherToMaybe . runEitherT . loadParent)
    $ commit
    where
        -- | Attempts to load the parent commit for a given commit.
        loadParent :: Commit -> EitherT Error IO Commit
        loadParent commit =
            if isJust $ parentHash commit
                then HIO.loadCommit
                    . fromJust
                    . parentHash
                    $ commit
                else left $ "No parent for commit with hash " ++
                    (Prelude.show . hash $ commit)

-- | Given any string, attempt to convert it to a hash.
--   Succeeds if the string is in a hash format, even if
--   the hash is not a key in the database (no commits / diffs
--   have been hashed with that key yet). Fails if the format
--   is unexpected. Acceptable formats are listed in user-facing
--   documentation.
refToHash :: String -> EitherT Error IO Hash
refToHash unparsedRef = do
    base <- case baseRef of
        "HEAD" -> HIO.loadHeadHash
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
        HIO.loadHeadHash >>= checkoutToDirectory headDir

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
        getTempDirectory
            = (map formatChar . Prelude.show) <$> getCurrentTime

        -- | Some characters can't be in directory names.
        formatChar :: Char -> Char
        formatChar ' ' = '-'
        formatChar '.' = '-'
        formatChar ':' = '-'
        formatChar ch = ch
