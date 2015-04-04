{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The standard commands accessible to users through the CLI
module Horse.Commands.Porcelain
( -- * Basic commands
  Horse.Commands.Porcelain.config
, Horse.Commands.Porcelain.init
, Horse.Commands.Porcelain.status
, Horse.Commands.Porcelain.stage
, Horse.Commands.Porcelain.checkout
, Horse.Commands.Porcelain.commit
, Horse.Commands.Porcelain.hshow
, Horse.Commands.Porcelain.log
) where

-- imports

import Prelude hiding (init, log)

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
    , putStrLn'
    , fromEitherMaybeDefault )

import qualified Horse.IO as HIO

-- | Sets user-specific configuration information.
config :: Maybe String -> Maybe Email -> IO ()
config maybeName maybeEmail = do
    configPath <- HIO.getConfigPath
    configFileExistedBefore <- Dir.doesFileExist configPath

    unless configFileExistedBefore $ do
        HIO.createFileWithContents (configPath, ByteString.empty) -- TODO

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
-- | one currently exists, it aborts.
init :: Maybe Verbosity -> IO ()
init maybeVerbosity = do
    let verbosity = fromMaybe Normal maybeVerbosity
    repositoryAlreadyExists <- Dir.doesDirectoryExist HIO.rootPath

    -- initialize config file; it's read from
    -- in this function and hence needs to exist
    config Nothing Nothing 

    when repositoryAlreadyExists
        $ putStrLn $ "Error: repository already exists"

    unless repositoryAlreadyExists $ do
        mapM_ HIO.destructivelyCreateDirectory HIO.directories

        let createOptions = DB.defaultOptions{ DB.createIfMissing = True }
        mapM_
            ((=<<) DBI.unsafeClose . (flip DB.open) createOptions)
            HIO.databasePaths

        sequence $ map HIO.createFileWithContents HIO.serializationPathsAndInitialContents

        currDir <- Dir.getCurrentDirectory
        unless (verbosity == Quiet) $
            putStrLn $ "Initialized existing horse-control repository in"
                ++ currDir ++ "/" ++ HIO.rootPath

-- | Prints the difference between the working directory and HEAD
-- TODO: check initialization has happened (all functions should do this)
status :: EitherT Error IO ()
status = do
    isRepository <- liftIO HIO.isRepositoryOrAncestorIsRepo
    unless isRepository $ do
        left $ "Fatal: Not a horse repository (or any of the ancestor directories)."

    HIO.loadStagingArea >>= (liftIO . print)

-- | Adds the specified modification / addition / deletion of the
-- | specified file to the staging area
stage :: String -> EitherT Error IO ()
stage path = do
    isRepository <- liftIO HIO.isRepositoryOrAncestorIsRepo
    unless isRepository $ do
        left $ "Fatal: Not a horse repository (or any of the ancestor directories)."

    stagingArea <- HIO.loadStagingArea

    let updatedStagingArea = case 0 of 0 -> stagingArea { adds = path : (adds stagingArea) }
                                       1 -> stagingArea { mods = path : (mods stagingArea) }
                                       2 -> stagingArea { dels = path : (dels stagingArea) }
                                       _   -> error "undefined flag"
    liftIO $ HIO.writeStagingArea updatedStagingArea

-- | Writes the staging area as a commit to disk. Currently takes a
-- | single parameter of a message.
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

    stagedDiff <- HIO.loadStagingArea >>= liftIO . HIO.getStagedDiff

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

    liftIO $ HIO.writeCommit completeCommit commitHash

    liftIO . HIO.writeHeadHash $ commitHash

    liftIO . HIO.writeStagingArea $ (Default.def :: StagingArea)

    unless (verbosity == Quiet) $ do
        putStrLn' $ "[<branch> "
            ++ (show . ByteString.take 8 $ hash completeCommit)
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

-- | Sets all tracked files to their state in the specified commit
-- | TODO: ref
checkout :: String -> EitherT Error IO ()
checkout ref = do
    isRepository <- liftIO HIO.isRepositoryOrAncestorIsRepo
    unless isRepository $ do
        left $ "Fatal: Not a horse repository (or any of the ancestor directories)."

    hash <- HIO.refToHash ref
    soughtCommit <- HIO.loadCommit hash
    ancestors <- HIO.loadHistory soughtCommit
    let diffs :: [FD.Diff] = map diffWithPrimaryParent ancestors
    mapM_ HIO.applyDiff (reverse diffs)

    putStrLn' $ "running command \"checkout\" with args "
    liftIO . print $ ref
    liftIO . print $ soughtCommit
    -- TODO

-- | Shows the specified commit. With no arguments, assumes a single
-- | argument of HEAD.
hshow :: Maybe String -> EitherT Error IO ()
hshow maybeRef = do
    isRepository <- liftIO HIO.isRepositoryOrAncestorIsRepo
    unless isRepository $ do
        left $ "Fatal: Not a horse repository (or any of the ancestor directories)."

    headHash <- HIO.loadHeadHash
    let ref = fromMaybe headHash (stringToHash <$> maybeRef)
    (HIO.loadCommit ref) >>= (liftIO . print)

-- | Prints the history from the current commit backwards
log :: Maybe String -> Maybe Int -> EitherT Error IO ()
log maybeRef maybeNumCommits = do
    isRepository <- liftIO HIO.isRepositoryOrAncestorIsRepo
    unless isRepository $ do
        left $ "Fatal: Not a horse repository (or any of the ancestor directories)."

    commitsHaveBeenMade <- HIO.commitsHaveBeenMade
    when commitsHaveBeenMade $ do
        headHash <- HIO.loadHeadHash

        let ref = maybe headHash stringToHash maybeRef

        commit <- HIO.loadCommit ref

        history <- (take <$> maybeNumCommits) |<$>| HIO.loadHistory commit
        (liftIO . print) $ message <$> history
