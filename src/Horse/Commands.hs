{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Commands exposed through the CLI
module Horse.Commands
( -- * changing HEAD
  Horse.Commands.checkout

  -- * set-up
, Horse.Commands.config
, Horse.Commands.init

  -- * displaying information
, Horse.Commands.log
, Horse.Commands.show
, Horse.Commands.diff

  -- * committing
, Horse.Commands.commit
, Horse.Commands.commitAmend
, Horse.Commands.squash

  -- * the staging area
, Horse.Commands.stage
, Horse.Commands.status
, Horse.Commands.unstage

  -- * untracking
, Horse.Commands.untrack
, Horse.Commands.retrack
, Horse.Commands.listUntrackedPaths

  -- * branches
, Horse.Commands.createBranch
, Horse.Commands.createBranchSetCurrent
, Horse.Commands.deleteBranch
, Horse.Commands.setBranch
, Horse.Commands.listBranches
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

import Control.Error.Util

-- qualified imports

import qualified Zora.List as ZL (take_while_keep_last)

import qualified Filediff as FD
import qualified Filediff.Stats as FD
import qualified Filediff.Types as FD

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBI

-- imported functions

import qualified Filesystem.Path (FilePath, collapse)

import Data.List
    ( groupBy 
    , sort
    , find
    , foldl'
    , (\\)
    , isInfixOf
    , isPrefixOf
    , nub )

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
    , maybeToEither
    , fromEitherMaybeDefault
    , (</>)
    , whenM
    , unlessM
    , toMaybe )
import qualified Horse.IO as HIO
import qualified Horse.Printing as HP
import qualified Horse.Constants as HC
import qualified Horse.Filesystem as HF

createBranchSetCurrent :: String -> Maybe String -> Maybe Verbosity -> EitherT Error IO Branch
createBranchSetCurrent branchName maybeRef maybeVerbosity =
    createBranch' branchName maybeRef maybeVerbosity True

createBranch :: String -> Maybe String -> Maybe Verbosity -> EitherT Error IO Branch
createBranch branchName maybeRef maybeVerbosity =
    createBranch' branchName maybeRef maybeVerbosity False

createBranch' :: String -> Maybe String -> Maybe Verbosity -> Bool -> EitherT Error IO Branch
createBranch' branchName maybeRef maybeVerbosity setCurrent = do
    let verbosity = fromMaybe Normal maybeVerbosity

    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    hash <- fromMaybe HIO.loadHeadHash (refToHash <$> maybeRef)

    let newBranch = Branch branchName hash False
    HIO.loadAllBranches >>= liftIO . HIO.writeAllBranches . (:) newBranch

    liftIO $ D.setCurrentDirectory userDirectory

    unless (verbosity == Quiet) $
        putStrLn' ("Created branch \"" ++ branchName ++ "\", pointing to commit " ++ (hashToString hash))

    right newBranch
    --where
    --    makeNotCurrent :: Branch -> Branch
    --    makeNotCurrent b@(Branch name hash _) = Branch name hash False

deleteBranch :: String -> Maybe Verbosity -> EitherT Error IO ()
deleteBranch branchNameToDelete maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    branches <- HIO.loadAllBranches
    let maybeBranchToDelete = find ((==) branchNameToDelete . branchName) branches
    when (isNothing maybeBranchToDelete) $
        left ("Error: can't delete nonexistent branch \"" ++ branchNameToDelete ++ "\"")
    let branchToDelete = fromJust maybeBranchToDelete

    let isCurrent = maybe False isCurrentBranch maybeBranchToDelete
    when isCurrent $
        left ("Fatal: cannot delete current branch (" ++ branchNameToDelete ++ ")")

    HIO.loadAllBranches >>= liftIO . HIO.writeAllBranches . (flip (\\) $ [branchToDelete])

    unless (verbosity == Quiet) $
        putStrLn' ("Deleted branch \"" ++ branchNameToDelete ++ "\"" ++ " (was " ++ (take 7 . hashToString $ branchHash branchToDelete) ++ ")")

    liftIO $ D.setCurrentDirectory userDirectory

listBranches :: Maybe Verbosity -> EitherT Error IO [Branch]
listBranches maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    branches <- HIO.loadAllBranches

    unless (verbosity == Quiet) $ liftIO . HP.printBranches $ branches

    liftIO $ D.setCurrentDirectory userDirectory

    right branches

setBranch :: String -> String -> Maybe Verbosity -> EitherT Error IO ()
setBranch branchName ref maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    liftIO $ D.setCurrentDirectory userDirectory

-- | Sets user-specific configuration information. The `Maybe String`
--   refers to the user's name.
config :: Maybe String -> Maybe EmailAddress -> EitherT Error IO Config
config maybeName maybeEmail = do
    liftIO $ do
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
                HF.createFileWithContents configPath BS.empty

                let userInfo = UserInfo {
                      name = fromMaybe Default.def maybeName
                    , email = fromMaybe Default.def maybeEmail }
                HIO.writeConfig $ Config { userInfo = userInfo }

    HIO.loadConfig

-- | Initializes an empty repository in the current directory. If
--   one currently exists, it aborts.
init :: Maybe Verbosity -> EitherT Error IO ()
init maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    whenM (liftIO $ HF.isInRepository ".") $
        left "Fatal: directory is or is subdirectory of another horse-control repo"

    config Nothing Nothing

    liftIO $ do
        -- initialize config file; it's read from
        -- in this function and hence needs to exist

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
    HF.assertIsRepositoryAndCdToRoot

    stagingArea <- HIO.loadStagingArea
    unstagedFiles <- loadUnstagedFiles
    let currentStatus = Status stagingArea unstagedFiles

    unless (verbosity == Quiet) $ do
        liftIO $ HP.printStatus currentStatus

    liftIO $ D.setCurrentDirectory userDirectory
    right currentStatus
    where
        -- | Gets all files that have modifications that are not staged.
        loadUnstagedFiles :: EitherT Error IO [FilePath]
        loadUnstagedFiles = do
            stagedFiles <- files <$> HIO.loadStagingArea
            untrackedPaths <- HIO.loadUntrackedPaths
            diffWithHEAD Nothing
                >>= right . sort . getUnstagedFilesFromDiff (stagedFiles ++ untrackedPaths)

        getUnstagedFilesFromDiff :: [FilePath] -> FD.Diff -> [FilePath]
        getUnstagedFilesFromDiff stagedOrUntrackedFiles
            = filter (\x -> none (isPrefixOf x) $ stagedOrUntrackedFiles)
            . map FD.comp
            . FD.filediffs

        none :: (a -> Bool) -> [a] -> Bool
        none f = not . any f

untrack :: String -> Maybe Verbosity -> EitherT Error IO ()
untrack path maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    -- TODO: figure out how to share this
    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    -- relative to root of repo
    relativePath <- HF.relativizePath path userDirectory
    when (isPrefixOf ".." relativePath) $ do
        left $ "Can't untrack file or directory outside of the repository: " ++ path

    HIO.loadUntrackedPaths >>= liftIO . HIO.writeUntrackedPaths . nub . (:) relativePath

    untrackingStagedFiles <- (any (flip isPrefixOf $ relativePath) . files) <$> HIO.loadStagingArea
    unless (verbosity == Quiet) $ do
        putStrLn' $ "Warning: some staged file(s) are subdirectories of or reside at the path you are trying to untrack. These files will not be removed from the staging area, but will be untracked for the future."

    liftIO $ D.setCurrentDirectory userDirectory

retrack :: String -> EitherT Error IO ()
retrack path = do
    -- TODO: figure out how to share this
    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    -- relative to root of repo
    relativePath <- HF.relativizePath path userDirectory
    when (isPrefixOf ".." relativePath) $ do
        left $ "Can't retrack file or directory outside of the repository: " ++ path

    HIO.loadUntrackedPaths >>= liftIO . HIO.writeUntrackedPaths . removeSubdirsOf relativePath

    liftIO $ D.setCurrentDirectory userDirectory
    where
        removeSubdirsOf :: FilePath -> [FilePath] -> [FilePath]
        removeSubdirsOf path = filter (not . isPrefixOf path)

listUntrackedPaths :: Maybe Verbosity -> EitherT Error IO [FilePath]
listUntrackedPaths maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    paths <- HIO.loadUntrackedPaths

    liftIO $ D.setCurrentDirectory userDirectory

    unless (verbosity == Quiet) $ do
        print' paths

    right paths

-- | Adds the whatever change was made (modification or addition or
--   deletion) to the specified file or directory to the staging area.
unstage :: String -> EitherT Error IO StagingArea
unstage path = do
    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    -- relative to root of repo
    relativePath <- HF.relativizePath path userDirectory
    when (isPrefixOf ".." relativePath) $ do
        left $ "Can't unstage file or directory outside of the repository: " ++ path
    relativePaths <- getRelativePaths relativePath
    -- TODO: share the above code

    let unstageFiles = mapStagingArea (flip (\\) $ relativePaths)
    stagingArea <- unstageFiles <$> HIO.loadStagingArea
    liftIO $ HIO.writeStagingArea stagingArea

    liftIO $ D.setCurrentDirectory userDirectory

    right stagingArea

-- | Adds the whatever change was made (modification or addition or
--   deletion) to the specified file or directory to the staging area.
stage :: String -> EitherT Error IO StagingArea
stage path = do
    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    -- relative to root of repo
    relativePath <- HF.relativizePath path userDirectory
    when (isPrefixOf ".." relativePath) $ do
        left $ "Can't stage file or directory outside of the repository: " ++ path
    when (relativePath == ".horse") $ do
        left $ "Fatal: cannot stage .horse; it is a directory required by horse-control."
    relativePaths <- getRelativePaths relativePath

    diffs <- FD.filediffs <$> (diffWithHEAD $ Just (relativePaths, True))

    pathExistsIsFile <- liftIO $ D.doesFileExist path
    pathExistsIsDir <- liftIO $ D.doesDirectoryExist path
    when (diffs == mempty && (not pathExistsIsFile) && not (pathExistsIsDir)) $
        -- if no file or directory exists at that path, but the diff
        -- is empty, then it wasn't deleted, and, hence, must be an
        -- invalid path.
        left $ "Can't stage file or directory at path \"" ++ path ++ "\"; no file or directory exists at that path, and no file was deleted at that path."

    let updateFunctions = zipWith ($) (repeat updateStagingArea) diffs

    stagingArea <- HIO.loadStagingArea
    let updatedStagingArea = foldl (flip ($)) stagingArea updateFunctions

    liftIO $ do
        HIO.writeStagingArea (mapStagingArea nub updatedStagingArea)
        D.setCurrentDirectory userDirectory

    right updatedStagingArea
    where
        updateStagingArea :: FD.Filediff -> StagingArea -> StagingArea
        updateStagingArea (FD.Filediff base _ change) stagingArea =
            case change of {
            FD.Add _ -> stagingArea { adds = base : (adds stagingArea) };
            FD.Mod _ -> stagingArea { mods = base : (mods stagingArea) };
            FD.Del _ -> stagingArea { dels = base : (dels stagingArea) };
            }

-- | Writes the changes housed in the staging area as a commit to disk,
--   then clears the staging area.
commitAmend :: CommitHasher -> Maybe String -> Maybe Verbosity -> EitherT Error IO Commit
commitAmend hasher maybeMessage maybeVerbosity = do
    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    unlessM commitsHaveBeenMade $
        left "Fatal: cannot amend when no commits have been made."

    latestCommit <- commit hasher maybeMessage maybeVerbosity
    squashedCommit <- squash hasher (hashToString . fromJust . parentHash $ latestCommit)

    liftIO $ D.setCurrentDirectory userDirectory
    right squashedCommit

-- | Writes the changes housed in the staging area as a commit to disk,
--   then clears the staging area. 'CommitHasher' is taken in for mocking
--   for testing purposes.
commit :: CommitHasher -> Maybe String -> Maybe Verbosity -> EitherT Error IO Commit
commit hasher maybeMessage maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity
    let message = fromMaybe "default message" maybeMessage

    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    -- commit params
    now <- liftIO $ fmap (toGregorian . utctDay) getCurrentTime

    isFirstCommit <- ((==) Default.def) <$> HIO.loadHeadHash
    parent <- if isFirstCommit
        then right Nothing
        else HIO.loadHeadHash >>= (fmap Just . HIO.loadCommit)

    stagingArea <- HIO.loadStagingArea
    when (isEmpty stagingArea) $
        left "Fatal: can't commit with an empty staging area."

    -- behavior of `stage` ensures that this will never be `mempty`
    stagedDiff <- HIO.loadStagingArea >>= diffWithHEAD . Just . (\fs -> (fs, False)) . files

    config <- HIO.loadConfig

    let hashlessCommit = Commit {
        author                  = userInfo config
        , date                  = now
        , hash                  = Default.def -- no hash yet since commit
                                              -- hasn't been created
        , parentHash            = hash <$> parent
        , diffWithPrimaryParent = stagedDiff
        , message               = message }

    let commitHash = hashingAlg hasher $ hashlessCommit
    let completeCommit = hashlessCommit { hash = commitHash }

    branches <- HIO.loadAllBranches
    let maybeCurrentBranch = find isCurrentBranch branches
    let currentBranch = maybe (Branch HC.defaultBranchName commitHash True) (\b -> b  { branchHash = commitHash }) maybeCurrentBranch
    let updatedBranches = currentBranch : (filter (not . isCurrentBranch) branches)
    liftIO $ HIO.writeAllBranches updatedBranches

    HIO.writeCommit completeCommit
    liftIO $ do
        HIO.writeHeadHash commitHash
        HIO.writeStagingArea (Default.def :: StagingArea)
        D.setCurrentDirectory userDirectory

    when (verbosity == Verbose) $ do
        liftIO $ HP.printCommit completeCommit

    unless (verbosity == Quiet) $ do
        liftIO $ HP.printCommitStats completeCommit

    right completeCommit

-- | Inclusive in param
squash :: CommitHasher -> String -> EitherT Error IO Commit
squash hasher ref = do
    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    historyToRoot <- log Nothing Nothing (Just Quiet)

    endHash <- refToHash ref
    let squashedCommit = getSquashedCommit historyToRoot endHash

    HIO.writeCommit squashedCommit
    liftIO $ do
        HIO.writeHeadHash (hash squashedCommit)
        D.setCurrentDirectory userDirectory

    right squashedCommit
    where
        getSquashedCommit :: [Commit] -> Hash -> Commit
        getSquashedCommit historyToRoot endHash =
            unhashedCommit { hash = hashingAlg hasher $ unhashedCommit }
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
checkout ref _ = do
    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    hash <- refToHash ref
    checkoutToDirectory "." hash

    liftIO $ HIO.writeHeadHash hash

    liftIO $ D.setCurrentDirectory userDirectory

-- | Prints information about the specified commit to the console. With
--   a `Nothing` for its parameter, it assumes a single argument of HEAD.
show :: Maybe String -> Maybe Verbosity -> EitherT Error IO Commit
show maybeRef maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    ref <- fromMaybe HIO.loadHeadHash (refToHash <$> maybeRef)
    commit <- HIO.loadCommit ref

    when (verbosity /= Quiet) $ liftIO (HP.printCommit commit)

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
    HF.assertIsRepositoryAndCdToRoot

    haveCommitsBeenMade <- commitsHaveBeenMade
    history <- if haveCommitsBeenMade
        then do
            commitHash <- maybe HIO.loadHeadHash refToHash maybeRef
            commit <- HIO.loadCommit commitHash
            history <- (take <$> maybeNumCommits) |<$>| loadHistory commit

            headHash <- HIO.loadHeadHash
            unless (verbosity == Quiet) $ do
                liftIO $ HP.printLog (Just headHash) history
            right history
        else right []

    liftIO $ D.setCurrentDirectory userDirectory

    right history

-- | Prints out the difference between the working directory and HEAD
diff :: Maybe Verbosity -> EitherT Error IO FD.Diff
diff maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    userDirectory <- liftIO D.getCurrentDirectory
    HF.assertIsRepositoryAndCdToRoot

    unlessM commitsHaveBeenMade $ do
        left "Fatal: can't diff with HEAD when no commits have been made."

    theDiff <- diffWithHEAD Nothing
    when (verbosity /= Quiet) $ do
        liftIO $ HP.printDiff theDiff

    liftIO $ D.setCurrentDirectory userDirectory

    right theDiff

-- * helper functions (not exposed)

-- * assorted

getRelativePaths :: FilePath -> EitherT Error IO [FilePath]
getRelativePaths relativePath = do
    isDir <- liftIO $ D.doesDirectoryExist relativePath

    unfiltered <- if isDir
        then liftIO $ (map (HF.collapse . (</>) relativePath)) <$> HF.getDirectoryContentsRecursiveSafe relativePath
        else right [relativePath]

    let filtered = filter (not . isInfixOf HC.repositoryDataDir) unfiltered

    right $ map (\p -> if "./" `isPrefixOf` p then drop 2 p else p) filtered

-- | Identifies whether any commits have been made in the current
--   repository.
commitsHaveBeenMade :: EitherT Error IO Bool
commitsHaveBeenMade = ((/=) Default.def) <$> HIO.loadHeadHash

-- | Checks out the specified hash to the specified directory. *NOTE*:
--   will entirely overwrite the contents of the specified directory;
--   be careful.
checkoutToDirectory :: FilePath -> Hash -> EitherT Error IO ()
checkoutToDirectory dir hash = do
    liftIO HF.assertCurrDirIsRepo

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
loadHistory commit = do
    liftIO HF.assertCurrDirIsRepo
    liftIO
        . fmap ((:) commit)
        . iterateMaybeM (runMaybeT . loadParent)
        $ commit
    where
        -- | Attempts to load the parent commit for a given commit.
        loadParent :: Commit -> MaybeT IO Commit
        loadParent commit =
            if isJust $ parentHash commit
                then hushT
                    . HIO.loadCommit
                    . fromJust
                    . parentHash
                    $ commit
                else MaybeT $ return Nothing

-- | Given any string, attempt to convert it to a hash.
--   Succeeds if the string is in a hash format, even if
--   the hash is not a key in the database (no commits / diffs
--   have been hashed with that key yet). Fails if the format
--   is unexpected. Acceptable formats are listed in user-facing
--   documentation.
refToHash :: String -> EitherT Error IO Hash
refToHash unparsedRef = do
    liftIO HF.assertCurrDirIsRepo

    base <- case baseRef of
        "HEAD" -> HIO.loadHeadHash
        someHash -> untruncateBaseRef someHash
    (hoistEither $ toAncestorDistance relatives) >>= applyRelatives base
    where
        parentSyntax :: Char
        parentSyntax = '^'

        ancestorSyntax :: Char
        ancestorSyntax = '~'

        -- throws exception if `read` fails
        toAncestorDistance :: String -> Either Error Int
        toAncestorDistance [] = Right 0
        toAncestorDistance r =
            if all ((==) parentSyntax) r
                then Right $ length r
                else if any ((==) parentSyntax) r
                    then Left $ "Fatal: cannot combine '" ++ [parentSyntax] ++ "' and '" ++ [ancestorSyntax] ++ "' syntax."
                    else Right $ read $ tail r

        applyRelatives :: Hash -> Int -> EitherT Error IO Hash
        applyRelatives h ancestorDistance = do
                history <- HIO.loadCommit h >>= loadHistory
                when (ancestorDistance >= length history) $
                    left "Fatal: specified relative commit is too far back in history; no commits exist there."
                right $ hash (history !! ancestorDistance)

        baseRef :: String
        baseRef = takeWhile (not . isRelativeSyntax) unparsedRef

        relatives :: String
        relatives = dropWhile (not . isRelativeSyntax) unparsedRef

        isRelativeSyntax :: Char -> Bool
        isRelativeSyntax ch
            = (ch == parentSyntax) || (ch == ancestorSyntax)

        untruncateBaseRef :: String -> EitherT Error IO Hash
        untruncateBaseRef baseRef = do
            when (Prelude.null baseRef) $
                left "Fatal: can't untruncate the empty hash."

            allHashes <- HIO.loadAllHashes
            let matchingHashes = filter ((==) baseRef . hashToString . BS.take (length baseRef)) allHashes

            allBranches <- HIO.loadAllBranches
            let matchingBranches = filter ((==) baseRef . branchName) allBranches

            let matching = matchingHashes ++ (map branchHash matchingBranches)

            case length matching of
                0 -> left $ "Fatal: ref " ++ (Prelude.show baseRef) ++ " does not match any branch names or stored hashes"
                1 -> right $ head matching
                _ -> left $ "Fatal: multiple hashes or branch names match specified ref: " ++ (Prelude.show matching)

-- * diffing

-- | Pass in `Nothing` to diff all files; otherwise, pass in
--   the files to diff.
diffWithHEAD :: Maybe ([FilePath], Bool) -> EitherT Error IO FD.Diff
diffWithHEAD maybeFilesToDiff = do
    liftIO HF.assertCurrDirIsRepo

    headDir <- liftIO $ ((</>) HC.repositoryDataDir) <$> getTempDirectory
    liftIO $ D.createDirectory headDir

    -- if no commits have been made; no point in filling the directory
    -- since HEAD doesn't exist so it would result in an empty diff
    -- anyway.
    whenM commitsHaveBeenMade $ do
        HIO.loadHeadHash >>= checkoutToDirectory headDir

    untrackedPaths <- listUntrackedPaths (Just Quiet)
    allFilesDiff <- liftIO $ FD.diffDirectoriesWithIgnoredSubdirs headDir "." [] ([HC.repositoryDataDir] ++ untrackedPaths)

    liftIO $ D.removeDirectoryRecursive headDir

    if isNothing maybeFilesToDiff
        then right allFilesDiff
        else do
            right
                $ FD.Diff
                . filter (shouldInclude (fromJust maybeFilesToDiff))
                . FD.filediffs
                $ allFilesDiff
    where
        shouldInclude :: ([FilePath], Bool) -> FD.Filediff -> Bool
        shouldInclude (filesToDiff, includeDeletions) filediff
            = ((flip elem $ filesToDiff) . FD.comp $ filediff)
            || (includeDeletions && (FD.isDel . FD.change $ filediff))
            -- `comp` instead of `base` because `comp` is ".", whereas
            -- `base` is ".horse/<temp directory>"

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
