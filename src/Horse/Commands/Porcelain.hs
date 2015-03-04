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

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)

-- horse-control imports

import Horse.Types

import qualified Horse.IO as HIO
import qualified Horse.Filesys as Filesys
import qualified Horse.Commands.Plumbing as Plumbing

unfoldrM :: Monad m => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldrM f b = do
    res <- f b
    case res of
        Just (a, b') -> do
            bs <- unfoldrM f b'
            return $ a : bs
        Nothing -> return []

myUnfoldrM :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
myUnfoldrM f v = (v:) `liftM` unfoldrM (liftM (fmap tuplefy) . f) v
    where tuplefy x = (x,x)

---------------------------------------

-- TODO: put this somewhere
fromMaybe' :: (Maybe a) -> a -> a
fromMaybe' = flip fromMaybe

-- | Sets user-specific configuration information.
config :: Maybe String -> Maybe Email -> IO ()
config maybeName maybeEmail = do
    configPath <- Filesys.getConfigPath
    configFileExistedBefore <- Dir.doesFileExist configPath

    unless configFileExistedBefore $ do
        Filesys.createFileWithContents (configPath, ByteString.empty) -- TODO

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
init :: IO ()
init = do
    repositoryAlreadyExists <- Dir.doesDirectoryExist Filesys.rootPath

    when repositoryAlreadyExists
        $ putStrLn "Error: repository already exists"

    unless repositoryAlreadyExists $ do
        mapM_ Filesys.destructivelyCreateDirectory Filesys.directories

        -- should close these DB handles?
        sequence $ map (flip DB.open $ DB.defaultOptions{ DB.createIfMissing = True }) Filesys.databasePaths

        sequence $ map Filesys.createFileWithContents Filesys.serializationPathsAndInitialContents

        currDir <- Dir.getCurrentDirectory
        putStrLn $ "Initialized existing horse-control repository in"
            ++ currDir ++ "/" ++ Filesys.rootPath

        return ()

-- | Prints the difference between the working directory and HEAD
-- TODO: check initialization has happened (all functions should do this)
status :: IO ()
status = do
    stagingArea <- HIO.loadStagingArea
    print stagingArea

-- | Adds the specified modification / addition / deletion of the
-- | specified file to the staging area
stage :: String -> IO ()
stage path = do
    stagingArea <- HIO.loadStagingArea

    let updatedStagingArea = case 0 of 0 -> stagingArea { adds = path : (adds stagingArea) }
                                       1 -> stagingArea { mods = path : (mods stagingArea) }
                                       2 -> stagingArea { dels = path : (dels stagingArea) }
                                       _   -> error "undefined flag"
    HIO.writeStagingArea updatedStagingArea

-- | Writes the staging area as a commit to disk. Currently takes a
-- | single parameter of a message.
commit :: Maybe String -> IO ()
commit maybeMessage = do
    now <- fmap (toGregorian . utctDay) getCurrentTime
    parent <- (headHash <$> HIO.loadHead) >>= HIO.loadCommit
    putStrLn $ "parent: " ++ (show parent)
    stagedDiff <- HIO.loadStagingArea >>= getStagedDiff
    config <- HIO.loadConfig
    let message = fromMaybe "default message" maybeMessage
    -- TODO: coalesce commit-creation somehow?
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

    HIO.writeCommit completeCommit commitHash

    HIO.writeHead $ Head { headHash = commitHash }

    HIO.writeStagingArea (Default.def :: StagingArea)

    -- debug code; can delete
    putStrLn "Testing writing of commit: loading written commit: "
    (HIO.loadCommit commitHash) >>= print

    putStrLn $ "[<branch> " ++ (show . ByteString.take 8 $ commitHash)
        ++  "] " ++ message
    putStrLn $ "0" ++ " files changed, " ++ "0" ++ " insertions(+), "
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
        getStagedDiff :: StagingArea -> IO Diff
        getStagedDiff stagingArea = return Default.def

-- | Sets all tracked files to their state in the specified
-- | TODO: ref
checkout :: String -> IO ()
checkout ref = do
    soughtCommit <- HIO.loadCommit . stringToHash $ ref
    putStrLn $ "running command \"checkout\" with args "
    print ref
    print soughtCommit
    -- TODO

-- | Shows the specified commit. With no arguments, assumes a single
-- | argument of HEAD.
hshow :: Maybe String -> IO ()
hshow maybeRef = do
    headHash <- headHash <$> HIO.loadHead
    let ref = fromMaybe headHash (stringToHash <$> maybeRef)
    ( HIO.loadCommit ref ) >>= print

log :: Maybe String -> Maybe Int -> IO ()
log maybeRef maybeNumCommits = do
    headHash <- headHash <$> HIO.loadHead
    let ref = fromMaybe headHash (stringToHash <$> maybeRef)
    maybeCommit <- HIO.loadCommit ref

    when (isJust maybeCommit) $ do
        let commit = fromJust maybeCommit

        history <- loadHistory maybeNumCommits commit
        print $ message <$> history
    where
        -- myUnfoldrM :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
        loadHistory :: Maybe Int -> Commit -> IO [Commit]
        loadHistory maybeNumCommits commit
            = myUnfoldrM loadParent commit

        loadParent :: Commit -> IO (Maybe Commit)
        loadParent commit =
            if isJust $ parentHash commit
                then HIO.loadCommit
                    . fromJust
                    . parentHash
                    $ commit
                else return Nothing
