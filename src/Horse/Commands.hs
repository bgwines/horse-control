{-# LANGUAGE InstanceSigs #-}
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
--, Horse.Commands.createBranchSetCurrent
, Horse.Commands.deleteBranch
, Horse.Commands.listBranches
, Horse.Commands.cherryPick
) where

-- imports

import Prelude hiding (init, log, show)
import qualified Prelude (show)

import GHC.Generics

import Data.Monoid
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

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

import Control.Conditional (whenM, unlessM)

import Data.Maybe (fromMaybe, isNothing, isJust, fromJust, catMaybes)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Filesystem.Path (parent, null)
import Filesystem.Path.CurrentOS (decodeString, encodeString)

-- horse-control imports

import Horse.Types
import Horse.Utils
    ( hashToString
    , (|<$>|)
    , note
    , fromEitherMaybeDefault
    , (</>)
    , toMaybe )
import qualified Horse.Refs as HR
import qualified Horse.IO as HIO
import qualified Horse.Printing as HP
import qualified Horse.Constants as HC
import qualified Horse.Filesystem as HF

executeCommand :: EIO a -> EIO a
executeCommand cmd = executeCommandPassingUserDirectory (const cmd)

executeCommandPassingUserDirectory :: (FilePath -> EIO a) -> EIO a
executeCommandPassingUserDirectory cmd = do
    userDirectory <- liftIO D.getCurrentDirectory

    HF.assertIsRepositoryAndCdToRoot

    result <- cmd userDirectory

    liftIO $ D.setCurrentDirectory userDirectory

    right result

createBranch :: String -> Maybe String -> Printer -> EIO Branch
createBranch branchName maybeRef printer = executeCommand (createBranch' branchName maybeRef printer)

createBranch' :: String -> Maybe String -> Printer -> EIO Branch
createBranch' branchName maybeRef (Printer _ putStrLn _ _ _) = do
    whenM (HR.isBranchRef branchName)
        (left ("Fatal: branch already exists: " ++ branchName))
    unlessM HIO.commitsHaveBeenMade
        (left "Fatal: cannot create branch when no commits have been made.")

    hash <- fromMaybe HIO.loadHeadHash (HR.refToHash <$> maybeRef)

    let newBranch = Branch branchName hash False
    HIO.loadAllBranches >>= liftIO . HIO.writeAllBranches . (:) newBranch

    liftIO . putStrLn $ "Created branch \"" ++ branchName ++ "\", pointing to commit " ++ hashToString hash

    right newBranch

deleteBranch :: String -> Printer -> EIO ()
deleteBranch branchNameToDelete printer = executeCommand (deleteBranch' branchNameToDelete printer)

deleteBranch' :: String -> Printer -> EIO ()
deleteBranch' branchNameToDelete (Printer _ putStrLn _ _ _) = do
    branches <- HIO.loadAllBranches
    let maybeBranchToDelete = find ((==) branchNameToDelete . branchName) branches
    when (isNothing maybeBranchToDelete) $
        left ("Error: can't delete nonexistent branch \"" ++ branchNameToDelete ++ "\"")
    let branchToDelete = fromJust maybeBranchToDelete

    when (isCurrentBranch branchToDelete) $
        left ("Fatal: cannot delete current branch (" ++ branchNameToDelete ++ ")")

    HIO.loadAllBranches >>= liftIO . HIO.writeAllBranches . flip (\\) [branchToDelete]

    liftIO . putStrLn $ ("Deleted branch \"" ++ branchNameToDelete ++ "\"" ++ " (was " ++ (take 7 . hashToString $ branchHash branchToDelete) ++ ")")

listBranches :: Printer -> EIO [Branch]
listBranches = executeCommand . listBranches'

listBranches' :: Printer -> EIO [Branch]
listBranches' printer = do
    branches <- HIO.loadAllBranches
    liftIO . HP.printBranches printer $ branches
    right branches

cherryPick :: String -> CommitHasher -> Printer -> EIO Commit
cherryPick ref hasher = executeCommand . cherryPick' ref hasher

cherryPick' :: String -> CommitHasher -> Printer -> EIO Commit
cherryPick' ref hasher printer = do
    commitToPick <- HR.refToHash ref >>= HIO.loadCommit

    -- TODO: verify diff can be applied

    now <- liftIO $ fmap (toGregorian . utctDay) getCurrentTime
    headHash <- HIO.loadHeadHash
    let hashlessCommit = Commit {
        author                  = author commitToPick 
        , date                  = now
        , hash                  = Default.def
        , parentHash            = Just headHash
        , diffWithPrimaryParent = diffWithPrimaryParent commitToPick
        , message               = message commitToPick }

    let commitHash = hashingAlg hasher hashlessCommit
    let completeCommit = hashlessCommit { hash = commitHash }

    branches <- HIO.loadAllBranches
    let maybeCurrentBranch = find isCurrentBranch branches
    when (isJust maybeCurrentBranch) $ do
        let currentBranch = fromJust maybeCurrentBranch
        let updatedBranch = currentBranch { branchHash = commitHash }
        let updatedBranches = updatedBranch : filter (not . isCurrentBranch) branches
        liftIO $ HIO.writeAllBranches updatedBranches

    HIO.writeCommit completeCommit
    liftIO $ do
        HIO.writeHeadHash commitHash
        HP.printCommitStats printer completeCommit

    right completeCommit

-- | Sets user-specific configuration information. The `Maybe String`
--   refers to the user's name.
config :: Maybe String -> Maybe EmailAddress -> EIO Config
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
                HIO.writeConfig Config { userInfo = updatedUserInfo }
            else do
                HF.createFileWithContents configPath BS.empty

                let userInfo = UserInfo {
                      name = fromMaybe Default.def maybeName
                    , email = fromMaybe Default.def maybeEmail }
                HIO.writeConfig Config { userInfo = userInfo }

    HIO.loadConfig

-- | Initializes an empty repository in the current directory. If
--   one currently exists, it aborts.
init :: Printer -> EIO ()
init (Printer _ putStrLn _ _ _) = do
    whenM (liftIO $ HF.isInRepository ".") $
        left "Fatal: directory is or is subdirectory of another horse-control repo"

    config Nothing Nothing

    liftIO $ do
        -- initialize config file; it's read from
        -- in this function and hence needs to exist

        mapM_ HF.destructivelyCreateDirectory HC.directories

        let createOptions = DB.defaultOptions{ DB.createIfMissing = True }
        mapM_ ((=<<) DBI.unsafeClose . flip DB.open createOptions)
            HC.databasePaths

        mapM_ (uncurry HF.createFileWithContents) HC.serializationPathsAndInitialContents

        currDir <- D.getCurrentDirectory
        liftIO . putStrLn $ "Initialized existing horse-control repository in" ++ currDir </> HC.repositoryDataDir

-- | Gets and prints the difference between the current state of the
-- filesystem and the state of the filesystem at HEAD.
status :: Printer -> EIO Status
status = executeCommand . status'

-- | Gets and prints the difference between the current state of the
-- filesystem and the state of the filesystem at HEAD.
status' :: Printer -> EIO Status
status' printer = do
    stagingArea <- HIO.loadStagingArea
    unstagedFiles <- loadUnstagedFiles
    let currentStatus = Status stagingArea unstagedFiles
    liftIO $ HP.printStatus printer currentStatus
    right currentStatus
    where
        -- | Gets all files that have modifications that are not staged.
        loadUnstagedFiles :: EIO [FilePath]
        loadUnstagedFiles = do
            stagedFiles <- files <$> HIO.loadStagingArea
            untrackedPaths <- HIO.loadUntrackedPaths
            let filesToIgnore = stagedFiles ++ untrackedPaths
            liftM (sort . getUnstagedFilesFromDiff filesToIgnore)
                diffWithHEADAllFiles

        getUnstagedFilesFromDiff :: [FilePath] -> FD.Diff -> [FilePath]
        getUnstagedFilesFromDiff stagedOrUntrackedFiles
            = filter (\x -> none (isPrefixOf x) stagedOrUntrackedFiles)
            . map FD.comp
            . FD.filediffs

        none :: (a -> Bool) -> [a] -> Bool
        none f = not . any f

untrack :: String -> Printer -> EIO ()
untrack path printer =
    executeCommandPassingUserDirectory (untrack' path printer)

untrack' :: String -> Printer -> FilePath -> EIO ()
untrack' path (Printer _ putStrLn _ _ _) userDirectory = do
    -- relative to root of repo
    relativePath <- HF.relativizePath path userDirectory
    when (".." `isPrefixOf` relativePath) $
        left $ "Can't untrack file or directory outside of the repository: " ++ path

    HIO.loadUntrackedPaths >>= liftIO . HIO.writeUntrackedPaths . nub . (:) relativePath

    untrackingStagedFiles <- (any (`isPrefixOf` relativePath) . files) <$> HIO.loadStagingArea
    when untrackingStagedFiles $
        liftIO . putStrLn $ "Warning: some staged file(s) are subdirectories of or reside at the path you are trying to untrack. These files will not be removed from the staging area, but will be untracked for the future."

retrack :: String -> EIO ()
retrack = executeCommandPassingUserDirectory . retrack'

retrack' :: String -> FilePath -> EIO ()
retrack' path userDirectory = do
    -- relative to root of repo
    relativePath <- HF.relativizePath path userDirectory
    when (".." `isPrefixOf` relativePath) $
        left $ "Can't retrack file or directory outside of the repository: " ++ path

    HIO.loadUntrackedPaths >>= liftIO . HIO.writeUntrackedPaths . removeSubdirsOf relativePath
    where
        removeSubdirsOf :: FilePath -> [FilePath] -> [FilePath]
        removeSubdirsOf path = filter (not . isPrefixOf path)

listUntrackedPaths :: Printer -> EIO [FilePath]
listUntrackedPaths = executeCommand . listUntrackedPaths'

listUntrackedPaths' :: Printer -> EIO [FilePath]
listUntrackedPaths' (Printer _ putStrLn _ _ _) = do
    paths <- HIO.loadUntrackedPaths
    liftIO . putStrLn $ Prelude.show paths
    right paths

-- | Adds the whatever change was made (modification or addition or
--   deletion) to the specified file or directory to the staging area.
unstage :: String -> EIO StagingArea
unstage = executeCommandPassingUserDirectory . unstage'

-- | Adds the whatever change was made (modification or addition or
--   deletion) to the specified file or directory to the staging area.
unstage' :: String -> FilePath -> EIO StagingArea
unstage' path userDirectory = do
    -- relative to root of repo
    relativePath <- HF.relativizePath path userDirectory
    when (".." `isPrefixOf` relativePath) $
        left $ "Can't unstage file or directory outside of the repository: " ++ path
    relativePaths <- getRelativePaths relativePath

    let unstageFiles = mapStagingArea (\\ relativePaths)
    stagingArea <- unstageFiles <$> HIO.loadStagingArea
    liftIO $ HIO.writeStagingArea stagingArea

    right stagingArea

-- | Adds the whatever change was made (modification or addition or
--   deletion) to the specified file or directory to the staging area.
stage :: String -> EIO StagingArea
stage = executeCommandPassingUserDirectory . stage'

-- | Adds the whatever change was made (modification or addition or
--   deletion) to the specified file or directory to the staging area.
stage' :: String -> FilePath -> EIO StagingArea
stage' path userDirectory = do
    -- relative to root of repo
    relativePath <- HF.relativizePath path userDirectory
    when (".." `isPrefixOf` relativePath) $
        left $ "Can't stage file or directory outside of the repository: " ++ path
    when (relativePath == ".horse") $
        left "Fatal: cannot stage .horse; it is a directory required by horse-control."
    relativePaths <- getRelativePaths relativePath

    diffs <- FD.filediffs <$> diffWithHEADIncludeDeletions relativePaths

    pathExistsIsFile <- liftIO $ D.doesFileExist path
    pathExistsIsDir <- liftIO $ D.doesDirectoryExist path
    when (diffs == mempty && not pathExistsIsFile && not pathExistsIsDir) $
        -- if no file or directory exists at that path, but the diff
        -- is empty, then it wasn't deleted, and, hence, must be an
        -- invalid path.
        left $ "Can't stage file or directory at path \"" ++ path ++ "\"; no file or directory exists at that path, and no file was deleted at that path."

    let updateFunctions = map updateStagingArea diffs

    stagingArea <- HIO.loadStagingArea
    let updatedStagingArea = foldl (flip ($)) stagingArea updateFunctions

    liftIO $ HIO.writeStagingArea (mapStagingArea nub updatedStagingArea)

    right updatedStagingArea
    where
        updateStagingArea :: FD.Filediff -> StagingArea -> StagingArea
        updateStagingArea (FD.Filediff base _ change) stagingArea =
            case change of {
            FD.Add _ -> stagingArea { adds = base : adds stagingArea };
            FD.Mod _ -> stagingArea { mods = base : mods stagingArea };
            FD.Del _ -> stagingArea { dels = base : dels stagingArea };
            }

-- | Writes the changes housed in the staging area as a commit to disk,
--   then clears the staging area.
commitAmend :: CommitHasher -> Maybe String -> Printer -> EIO Commit
commitAmend hasher maybeMessage printer =
    executeCommand (commitAmend' hasher maybeMessage printer)

-- | Writes the changes housed in the staging area as a commit to disk,
--   then clears the staging area.
commitAmend' :: CommitHasher -> Maybe String -> Printer -> EIO Commit
commitAmend' hasher maybeMessage printer = do
    unlessM HIO.commitsHaveBeenMade $
        left "Fatal: cannot amend when no commits have been made."

    latestCommit <- commit hasher maybeMessage printer
    squash hasher (hashToString . fromJust . parentHash $ latestCommit)

-- | Writes the changes housed in the staging area as a commit to disk,
--   then clears the staging area. 'CommitHasher' is taken in for mocking
--   for testing purposes.
commit :: CommitHasher -> Maybe String -> Printer -> EIO Commit
commit hasher maybeMessage printer =
    executeCommand (commit' hasher maybeMessage printer)

-- | Writes the changes housed in the staging area as a commit to disk,
--   then clears the staging area. 'CommitHasher' is taken in for mocking
--   for testing purposes.
commit' :: CommitHasher -> Maybe String -> Printer -> EIO Commit
commit' hasher maybeMessage printer = do
    let message = fromMaybe "default message" maybeMessage

    -- commit params
    now <- liftIO $ fmap (toGregorian . utctDay) getCurrentTime

    isFirstCommit <- (==) Default.def <$> HIO.loadHeadHash
    parent <- if isFirstCommit
        then right Nothing
        else HIO.loadHeadHash >>= (fmap Just . HIO.loadCommit)

    stagingArea <- HIO.loadStagingArea
    when (isEmpty stagingArea) $
        left "Fatal: can't commit with an empty staging area."

    stagedDiff <- diffWithHEADDoNotIncludeDeletions (files stagingArea)

    config <- HIO.loadConfig

    let hashlessCommit = Commit {
        author                  = userInfo config
        , date                  = now
        , hash                  = Default.def -- no hash yet since commit
                                              -- hasn't been created
        , parentHash            = hash <$> parent
        , diffWithPrimaryParent = stagedDiff
        , message               = message }

    let commitHash = hashingAlg hasher hashlessCommit
    let completeCommit = hashlessCommit { hash = commitHash }

    branches <- HIO.loadAllBranches
    let maybeCurrentBranch = find isCurrentBranch branches
    let shouldUpdateABranch = isFirstCommit || isJust maybeCurrentBranch
    when shouldUpdateABranch $ do
        let currentBranch = maybe (Branch HC.defaultBranchName commitHash True) (\b -> b  { branchHash = commitHash }) maybeCurrentBranch
        let updatedBranches = currentBranch : filter (not . isCurrentBranch) branches
        liftIO $ HIO.writeAllBranches updatedBranches

    HIO.writeCommit completeCommit
    liftIO $ do
        HIO.writeHeadHash commitHash
        HIO.writeStagingArea (Default.def :: StagingArea)
        HP.printCommitStats printer completeCommit

    right completeCommit

-- | Inclusive in param
squash :: CommitHasher -> String -> EIO Commit
squash hasher ref = executeCommand (squash' hasher ref)

-- | Inclusive in param
squash' :: CommitHasher -> String -> EIO Commit
squash' hasher ref = do
    historyToRoot <- log Nothing Nothing quietPrinter

    endHash <- HR.refToHash ref
    let squashedCommit = getSquashedCommit historyToRoot endHash

    HIO.writeCommit squashedCommit
    liftIO $ HIO.writeHeadHash (hash squashedCommit)

    right squashedCommit
    where
        getSquashedCommit :: [Commit] -> Hash -> Commit
        getSquashedCommit historyToRoot endHash =
            unhashedCommit { hash = hashingAlg hasher unhashedCommit }
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
checkout :: String -> EIO ()
checkout = executeCommand . checkout'

-- | Sets the contents of the filesystem to the state it had in the
--   specified commit.
checkout' :: String -> EIO ()
checkout' ref = do
    hash <- HR.refToHash ref
    checkoutToDirectory "." hash
    liftIO $ HIO.writeHeadHash hash

    -- if checking out a branch, make it current; otherwise,
    -- make all branches not current.
    isBranch <- HR.isBranchRef ref
    updatedBranches <- if isBranch
        then do
            branch <- HR.loadBranchFromRef ref
            allBranches <- HIO.loadAllBranches
            let otherBranches = filter ((/=) ref . branchName) allBranches
            right (makeCurrent branch : map makeNotCurrent otherBranches)
        else map makeNotCurrent <$> HIO.loadAllBranches
    liftIO $ HIO.writeAllBranches updatedBranches

-- | Prints information about the specified commit to the console. With
--   a `Nothing` for its parameter, it assumes a single argument of HEAD.
show :: Maybe String -> Printer -> EIO Commit
show maybeRef printer = executeCommand (show' maybeRef printer)

-- | Prints information about the specified commit to the console. With
--   a `Nothing` for its parameter, it assumes a single argument of HEAD.
show' :: Maybe String -> Printer -> EIO Commit
show' maybeRef printer = do
    ref <- fromMaybe HIO.loadHeadHash (HR.refToHash <$> maybeRef)
    commit <- HIO.loadCommit ref
    liftIO (HP.printCommit printer commit)
    right commit

-- | Prints the history from the current commit backwards. With
--   a `Nothing` for its parameter, it assumes a single argument of HEAD.
--   Pass in a `Just` `Int` to specify the number of commits back to go
--   in the history.
log :: Maybe String -> Maybe Int -> Printer -> EIO [Commit]
log maybeRef maybeNumCommits printer =
    executeCommand (log' maybeRef maybeNumCommits printer)

-- | Prints the history from the current commit backwards. With
--   a `Nothing` for its parameter, it assumes a single argument of HEAD.
--   Pass in a `Just` `Int` to specify the number of commits back to go
--   in the history.
log' :: Maybe String -> Maybe Int -> Printer -> EIO [Commit]
log' maybeRef maybeNumCommits printer = do
    haveCommitsBeenMade <- HIO.commitsHaveBeenMade
    history <- if haveCommitsBeenMade
        then do
            commitHash <- maybe HIO.loadHeadHash HR.refToHash maybeRef
            commit <- HIO.loadCommit commitHash
            history <- (take <$> maybeNumCommits) |<$>| HIO.loadHistory commit

            headHash <- HIO.loadHeadHash
            HIO.loadAllBranches >>= liftIO . HP.printLog printer headHash history
            right history
        else right []

    right history

-- | Prints out the difference between the working directory and HEAD
diff :: Printer -> EIO FD.Diff
diff = executeCommand . diff'

-- | Prints out the difference between the working directory and HEAD
diff' :: Printer -> EIO FD.Diff
diff' printer = do
    unlessM HIO.commitsHaveBeenMade $
        left "Fatal: can't diff with HEAD when no commits have been made."

    theDiff <- diffWithHEADAllFiles
    liftIO $ HP.printDiff printer theDiff
    right theDiff

-- * helper functions (not exposed)

-- * assorted

getRelativePaths :: FilePath -> EIO [FilePath]
getRelativePaths relativePath = do
    isDir <- liftIO $ D.doesDirectoryExist relativePath

    unfiltered <- if isDir
        then liftIO $ map (HF.collapse . (</>) relativePath) <$> HF.getDirectoryContentsRecursiveSafe relativePath
        else right [relativePath]

    let filtered = filter (not . isInfixOf HC.repositoryDataDir) unfiltered

    right $ map (\p -> if "./" `isPrefixOf` p then drop 2 p else p) filtered

-- | Checks out the specified hash to the specified directory. *NOTE*:
--   will entirely overwrite the contents of the specified directory;
--   be careful.
checkoutToDirectory :: FilePath -> Hash -> EIO ()
checkoutToDirectory dir hash = do
    liftIO HF.assertCurrDirIsRepo

    liftIO clearDirectory
    history <- HIO.loadCommit hash >>= HIO.loadHistory
    let diffs = reverse . map diffWithPrimaryParent $ history
    let diffWithRoot = foldl' mappend mempty diffs

    void (FD.applyDirectoryDiff diffWithRoot dir)
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

-- * diffing

diffWithHEADIncludeDeletions :: [FilePath] -> EIO FD.Diff
diffWithHEADIncludeDeletions filesToDiff = diffWithHEAD' (Just (filesToDiff, True))

diffWithHEADDoNotIncludeDeletions :: [FilePath] -> EIO FD.Diff
diffWithHEADDoNotIncludeDeletions filesToDiff = diffWithHEAD' (Just (filesToDiff, False))

diffWithHEADAllFiles :: EIO FD.Diff
diffWithHEADAllFiles = diffWithHEAD' Nothing

-- | Pass in `Nothing` to diff all files; otherwise, pass in
--   the files to diff.
diffWithHEAD' :: Maybe ([FilePath], Bool) -> EIO FD.Diff
diffWithHEAD' maybeFilesToDiff = do
    liftIO HF.assertCurrDirIsRepo

    headDir <- liftIO $ (HC.repositoryDataDir </>) <$> getTempDirectory
    liftIO $ D.createDirectory headDir

    -- if no commits have been made; no point in filling the directory
    -- since HEAD doesn't exist so it would result in an empty diff
    -- anyway.
    whenM HIO.commitsHaveBeenMade $
        HIO.loadHeadHash >>= checkoutToDirectory headDir

    untrackedPaths <- listUntrackedPaths quietPrinter
    allFilesDiff <- liftIO $ FD.diffDirectoriesWithIgnoredSubdirs headDir "." [] (HC.repositoryDataDir : untrackedPaths)

    liftIO $ D.removeDirectoryRecursive headDir

    case maybeFilesToDiff of
        Nothing -> right allFilesDiff
        (Just filesToDiff) -> right
            . FD.Diff
            . filter (shouldInclude filesToDiff)
            . FD.filediffs
            $ allFilesDiff
    where
        shouldInclude :: ([FilePath], Bool) -> FD.Filediff -> Bool
        shouldInclude (filesToDiff, includeDeletions) filediff
            = (includeDeletions && (FD.isDel . FD.change $ filediff))
            || ((`elem` filesToDiff) . FD.comp $ filediff)
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
