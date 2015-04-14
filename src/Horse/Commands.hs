{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Commands exposed through the CLI
module Horse.Commands
( -- * Basic commands
  Horse.Commands.config
, Horse.Commands.init
, Horse.Commands.status
, Horse.Commands.stage
, Horse.Commands.commit
, Horse.Commands.checkout
, Horse.Commands.show
, Horse.Commands.log
) where

-- imports

import Prelude hiding (init, log, show)
import qualified Prelude (show)

import GHC.Generics

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either

-- qualified imports

import qualified Filediff as FD
import qualified Filediff.Types as FD

import qualified System.IO as IO
import qualified System.Directory as Dir

import qualified Data.Hex as Hex
import qualified Data.Default as Default
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString

import qualified Crypto.Hash.SHA256 as SHA256

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBI

-- imported functions

import Data.Maybe (fromMaybe, isNothing, isJust, fromJust, catMaybes)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)

-- horse-control imports

import Horse.Types
import Horse.Utils
    ( stringToHash
    , (|<$>|)
    , print'
    , putStrLn'
    , fromEitherMaybeDefault )
import qualified Horse.IO as HIO
import qualified Horse.Filesystem as HF
import qualified Horse.Constants as HC

-- | Sets user-specific configuration information. The `Maybe String`
--   refers to the user's name.
config :: Maybe String -> Maybe EmailAddress -> IO ()
config maybeName maybeEmail = do
    configPath <- HC.configPath
    configFileExistedBefore <- Dir.doesFileExist configPath

    unless configFileExistedBefore $ do
        HF.createFileWithContents configPath ByteString.empty

        let userInfo = UserInfo {
              name = fromMaybe Default.def maybeName
            , email = fromMaybe Default.def maybeEmail }
        HIO.writeConfig $ Config { userInfo = userInfo }

    when configFileExistedBefore $ do
        eitherUserInfo <- runEitherT $ userInfo <$> HIO.loadConfig
        let updatedUserInfo = UserInfo {
              name = fromEitherMaybeDefault
                (name <$> eitherUserInfo)
                maybeName
            , email = fromEitherMaybeDefault
                (email <$> eitherUserInfo)
                maybeEmail }
        HIO.writeConfig $ Config { userInfo = updatedUserInfo }

-- | Initializes an empty repository in the current directory. If
--   one currently exists, it aborts.
init :: Maybe Verbosity -> IO ()
init maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity
    repositoryAlreadyExists <- Dir.doesDirectoryExist HC.repositoryDataDir

    -- initialize config file; it's read from
    -- in this function and hence needs to exist
    config Nothing Nothing 

    when repositoryAlreadyExists
        $ putStrLn $ "Error: repository already exists"

    unless repositoryAlreadyExists $ do
        mapM_ HF.destructivelyCreateDirectory HC.directories

        let createOptions = DB.defaultOptions{ DB.createIfMissing = True }
        mapM_
            ((=<<) DBI.unsafeClose . (flip DB.open) createOptions)
            HC.databasePaths

        sequence $ map (uncurry HF.createFileWithContents) HC.serializationPathsAndInitialContents

        currDir <- Dir.getCurrentDirectory
        unless (verbosity == Quiet) $
            putStrLn $ "Initialized existing horse-control repository in"
                ++ currDir ++ "/" ++ HC.repositoryDataDir

-- | Gets and prints the difference between the current state of the
-- filesystem and the state of the filesystem at HEAD.
status :: Maybe Verbosity -> EitherT Error IO Status
status maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    isRepository <- liftIO HIO.isRepositoryOrAncestorIsRepo
    unless isRepository $ do
        left $ "Fatal: Not a horse repository (or any of the ancestor directories)."

    stagingArea <- HIO.loadStagingArea
    unstagedFiles <- HIO.loadUnstagedFiles
    let currentStatus = Status stagingArea unstagedFiles

    unless (verbosity == Quiet) $ do
        putStrLn' "Staged changes:"
        liftIO . print $ stagingArea
        putStrLn' ""

        putStrLn' "Unstaged changes:"
        liftIO . print $ unstagedFiles

    return currentStatus

-- | Adds the whatever change was made (modification or addition or
--   deletion) to the specified file to the staging area.
stage :: String -> EitherT Error IO StagingArea
stage path = do
    isRepository <- liftIO HIO.isRepositoryOrAncestorIsRepo
    unless isRepository $ do
        left $ "Fatal: Not a horse repository (or any of the ancestor directories)."

    change <- (FD.change . head . FD.filediffs) <$> (HIO.diffWithHEAD (Just [path]))

    stagingArea <- HIO.loadStagingArea
    let updatedStagingArea = case change of FD.Add _ -> stagingArea { adds = path : (adds stagingArea) }
                                            FD.Mod _ -> stagingArea { mods = path : (mods stagingArea) }
                                            FD.Del _ -> stagingArea { dels = path : (dels stagingArea) }

    liftIO $ HIO.writeStagingArea updatedStagingArea
    right updatedStagingArea

-- | Writes the changes housed in the staging area as a commit to disk,
--   then clears the staging area.
commit :: Maybe String -> Maybe Verbosity -> EitherT Error IO Commit
commit maybeMessage maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity
    let message = fromMaybe "default message" maybeMessage

    isRepository <- liftIO HIO.isRepositoryOrAncestorIsRepo
    unless isRepository $ do
        left $ "Fatal: Not a horse repository (or any of the ancestor directories)."

    now <- liftIO $ fmap (toGregorian . utctDay) getCurrentTime

    isFirstCommit <- ((==) Default.def) <$> HIO.loadHeadHash
    parent <- if isFirstCommit
        then right Nothing
        else HIO.loadHeadHash >>= (fmap Just . HIO.loadCommit)

    stagedDiff <- HIO.loadStagingArea >>= HIO.getStagedDiff

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

    liftIO $ HIO.writeCommit completeCommit

    liftIO $ HIO.writeHeadHash commitHash

    liftIO $ HIO.writeStagingArea (Default.def :: StagingArea)

    when (verbosity == Verbose) $ do
        print' completeCommit

    unless (verbosity == Quiet) $ do
        putStrLn' $ "[<branch> "
            ++ (Prelude.show . ByteString.take 8 $ commitHash)
            ++  "] " ++ message
        putStrLn' $ "0" ++ " files changed, "
            ++ "0" ++ " insertions(+), "
            ++ "0" ++ " deletions(-)"

    right completeCommit
    where
        -- TODO: where does this go?
        hashCommit :: Commit -> Hash
        hashCommit
            = ByteString.take 40
            . Hex.hex
            . SHA256.hash
            . Serialize.encode

-- | Sets the contents of the filesystem to the state it had in the
--   specified commit.
checkout :: String -> Maybe Verbosity -> EitherT Error IO ()
checkout ref maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    isRepository <- liftIO HIO.isRepositoryOrAncestorIsRepo
    unless isRepository $ do
        left $ "Fatal: Not a horse repository (or any of the ancestor directories)."

    HIO.refToHash ref >>= HIO.checkoutToDirectory "."

-- | Prints information about the specified commit to the console. With
--   a `Nothing` for its parameter, it assumes a single argument of HEAD.
show :: Maybe String -> EitherT Error IO ()
show maybeRef = do
    isRepository <- liftIO HIO.isRepositoryOrAncestorIsRepo
    unless isRepository $ do
        left $ "Fatal: Not a horse repository (or any of the ancestor directories)."

    headHash <- HIO.loadHeadHash
    let ref = fromMaybe headHash (stringToHash <$> maybeRef)
    (HIO.loadCommit ref) >>= (liftIO . print)

-- | Prints the history from the current commit backwards. With
--   a `Nothing` for its parameter, it assumes a single argument of HEAD.
--   Pass in a `Just` `Int` to specify the number of commits back to go
--   in the history.
log :: Maybe String -> Maybe Int -> Maybe Verbosity -> EitherT Error IO [Commit]
log maybeRef maybeNumCommits maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity

    isRepository <- liftIO HIO.isRepositoryOrAncestorIsRepo
    unless isRepository $ do
        left $ "Fatal: Not a horse repository (or any of the ancestor directories)."

    commitsHaveBeenMade <- HIO.commitsHaveBeenMade
    if commitsHaveBeenMade
        then do
            headHash <- HIO.loadHeadHash
            let ref = maybe headHash stringToHash maybeRef

            commit <- HIO.loadCommit ref
            history <- (take <$> maybeNumCommits) |<$>| HIO.loadHistory commit

            unless (verbosity == Quiet) $ do
                liftIO . print $ hash <$> history
            right history
        else right []
