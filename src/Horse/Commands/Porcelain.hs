{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

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

import qualified Data.Convertible as Convert

import qualified System.IO as IO
import qualified System.Directory as Dir

import qualified Data.Hex as Hex
import qualified Data.Default as Default
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString

import qualified Crypto.Hash.SHA256 as SHA256

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

-- imported functions

import Data.Maybe (fromMaybe, isNothing, isJust, fromJust, catMaybes)
import Data.Either.Unwrap (fromLeft, fromRight)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Data.Generics.Aliases (orElse)

import Text.Printf (printf)

import Control.Monad.Extra (iterateMaybeM)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)

-- horse-control imports

import Horse.Types

import qualified Horse.IO as HIO
import qualified Horse.Utils as Utils

-- | Sets user-specific configuration information.
config :: Maybe String -> Maybe Email -> EitherT Error IO ()
config maybeName maybeEmail = do
    configPath <- HIO.getConfigPath
    configFileExistedBefore <- liftIO $ Dir.doesFileExist configPath

    unless configFileExistedBefore $ do
        HIO.createFileWithContents (configPath, ByteString.empty) -- TODO

        let userInfo = UserInfo {
              name = fromMaybe Default.def maybeName
            , email = fromMaybe Default.def maybeEmail }
        HIO.writeConfig $ Config { userInfo = userInfo }

    when configFileExistedBefore $ do
        userInfo <- userInfo <$> HIO.loadConfig
        let updatedUserInfo = UserInfo {
              name = fromMaybe (name userInfo) maybeName
            , email = fromMaybe (email userInfo) maybeEmail }
        HIO.writeConfig $ Config { userInfo = updatedUserInfo }

-- | Initializes an empty repository in the current directory. If
-- | one currently exists, it aborts.
init :: EitherT Error IO ()
init = do
    repositoryAlreadyExists <- liftIO $ Dir.doesDirectoryExist HIO.rootPath

    when repositoryAlreadyExists
        $ liftIO . putStrLn $ "Error: repository already exists"

    unless repositoryAlreadyExists $ do
        mapM_ HIO.destructivelyCreateDirectory HIO.directories

        -- should close these DB handles?
        liftIO . sequence $ map (flip DB.open $ DB.defaultOptions{ DB.createIfMissing = True }) HIO.databasePaths

        sequence $ map HIO.createFileWithContents HIO.serializationPathsAndInitialContents

        currDir <- liftIO Dir.getCurrentDirectory
        liftIO . putStrLn $ "Initialized existing horse-control repository in"
            ++ currDir ++ "/" ++ HIO.rootPath

-- | Prints the difference between the working directory and HEAD
-- TODO: check initialization has happened (all functions should do this)
status :: EitherT Error IO ()
status = do
    HIO.loadStagingArea >>= (liftIO . print)

-- | Adds the specified modification / addition / deletion of the
-- | specified file to the staging area
stage :: String -> EitherT Error IO ()
stage path = do
    stagingArea <- HIO.loadStagingArea

    let updatedStagingArea = case 0 of 0 -> stagingArea { adds = path : (adds stagingArea) }
                                       1 -> stagingArea { mods = path : (mods stagingArea) }
                                       2 -> stagingArea { dels = path : (dels stagingArea) }
                                       _   -> error "undefined flag"
    HIO.writeStagingArea updatedStagingArea

-- | Writes the staging area as a commit to disk. Currently takes a
-- | single parameter of a message.
commit :: Maybe String -> EitherT Error IO ()
commit maybeMessage = do
    now <- liftIO $ fmap (toGregorian . utctDay) getCurrentTime
    parent <- (headHash <$> HIO.loadHead) >>= HIO.loadCommit
    liftIO . putStrLn $ "parent: " ++ (show parent)
    stagedDiff <- HIO.loadStagingArea >>= getStagedDiff
    config <- HIO.loadConfig
    let message = fromMaybe "default message" maybeMessage
    -- TODO: coalesce commit-creation somehow?
    let hashlessCommit = Commit {
        author                  = userInfo config
        , date                  = now
        , hash                  = Default.def -- no hash yet since commit
                                              -- hasn't been created
        , parentHash            = Just $ hash parent
        , diffWithPrimaryParent = stagedDiff
        , message               = message }

    let commitHash = hashCommit hashlessCommit
    let completeCommit = hashlessCommit { hash = commitHash }

    HIO.writeCommit completeCommit commitHash

    HIO.writeHead $ Head { headHash = commitHash }

    HIO.writeStagingArea (Default.def :: StagingArea)

    -- debug code; can delete
    liftIO . putStrLn $ "Testing writing of commit: loading written commit: "
    (HIO.loadCommit commitHash) >>= (liftIO . print)

    liftIO . putStrLn $ "[<branch> " ++ (show . ByteString.take 8 $ commitHash)
        ++  "] " ++ message
    liftIO . putStrLn $ "0" ++ " files changed, " ++ "0" ++ " insertions(+), "
        ++ "0" ++ " deletions(-)"

    where
        -- TODO: where does this go?
        hashCommit :: Commit -> Hash
        hashCommit
            = ByteString.take 40
            . Hex.hex
            . SHA256.hash
            . Serialize.encode

        -- TODO
        getStagedDiff :: StagingArea -> EitherT Error IO Diff
        getStagedDiff stagingArea = return Default.def

-- | Sets all tracked files to their state in the specified commit
-- | TODO: ref
checkout :: String -> EitherT Error IO ()
checkout ref = do
    soughtCommit <- HIO.loadCommit . stringToHash $ ref
    liftIO . putStrLn $ "running command \"checkout\" with args "
    liftIO . print $ ref
    liftIO . print $ soughtCommit
    -- TODO

-- | Shows the specified commit. With no arguments, assumes a single
-- | argument of HEAD.
hshow :: Maybe String -> EitherT Error IO ()
hshow maybeRef = do
    headHash <- headHash <$> HIO.loadHead
    let ref = fromMaybe headHash (stringToHash <$> maybeRef)
    ( HIO.loadCommit ref ) >>= (liftIO . print)

-- | Prints the history from the current commit backwards
log :: Maybe String -> Maybe Int -> EitherT Error IO ()
log maybeRef maybeNumCommits = do
    headHash <- headHash <$> HIO.loadHead
    let ref = fromMaybe headHash (stringToHash <$> maybeRef)
    commit <- HIO.loadCommit ref
    history <- loadHistory maybeNumCommits commit
    (liftIO . print) $ message <$> history
    where
        loadHistory :: Maybe Int -> Commit -> EitherT Error IO [Commit]
        loadHistory maybeNumCommits commit
            = liftIO
            $ iterateMaybeM
                (fmap Utils.eitherToMaybe . runEitherT . loadParent)
                commit

        loadParent :: Commit -> EitherT Error IO Commit
        loadParent commit =
            if isJust $ parentHash commit
                then HIO.loadCommit
                    . fromJust
                    . parentHash
                    $ commit
                else left $ "No parent for commit with hash " ++ (show . hash $ commit)
