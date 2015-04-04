{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- imports

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

-- qualified imports

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

import System.Exit (exitSuccess)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Control.Monad ((>>=), return, when)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)

import Data.Either.Combinators (isLeft, fromLeft)

-- horse imports

import Horse.Types

import qualified Horse.IO as HIO
import qualified Horse.Commands.Porcelain as H

-- | Runs a test in its own empty directory.
-- | Effectively, it isolates it from all other tests.
runTest :: Assertion -> Assertion
runTest t = do
    testDirectory <- getTestDirectory
    Dir.createDirectory testDirectory
    Dir.setCurrentDirectory testDirectory

    t

    Dir.setCurrentDirectory ".."
    Dir.removeDirectoryRecursive testDirectory
    where
        -- | Gives a name of a directory that is pretty much guaranteed to
        -- | exist, so it's free for creation.
        getTestDirectory :: IO FilePath
        getTestDirectory = (map formatChar . show) <$> getCurrentTime
            where
                -- | Some characters can't be in directory names.
                formatChar :: Char -> Char
                formatChar ' ' = '-'
                formatChar '.' = '-'
                formatChar ':' = '-'
                formatChar ch = ch

quietCommit :: Maybe String -> EitherT Error IO Commit
quietCommit = (flip H.commit $ Just Quiet)

-- | Test the `horse log` command
testLog :: Assertion
testLog = do
    H.init (Just Quiet)
    eitherSuccess <- runEitherT $ do
        let messages = map Just ["A", "B", "C", "D"]
        commits <- mapM quietCommit messages

        history <- reverse <$> HIO.loadHistory (last commits)

        liftIO $ commits @?= history
        liftIO $ (length commits) @?= (length messages) -- WLOG
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft "" eitherSuccess)
    return ()

-- | Test the `horse stage` command
testStage :: Assertion
testStage = do
    H.init (Just Quiet)

    let addedFile = "a"
    handle <- IO.openFile addedFile IO.WriteMode
    IO.hPutStr handle "a"
    IO.hClose handle

    runEitherT $ H.stage addedFile

    eitherStagingArea <- runEitherT HIO.loadStagingArea

    (Right [addedFile]) @?= (adds <$> eitherStagingArea)

    (Right [])          @?= (mods <$> eitherStagingArea)

    (Right [])          @?= (dels <$> eitherStagingArea)

-- | Test the `horse init` command
testInit :: Assertion
testInit = do
    H.init (Just Quiet)
    rootDirectoryCreated <- Dir.doesDirectoryExist HIO.rootPath
    rootDirectoryCreated @?= True

-- | Verify that a command failed gracefully, as it should when run
-- | without a repository in which to execute
testNoRepo :: EitherT Error IO a -> Assertion
testNoRepo = (=<<) ((@?=) True . isLeft) . runEitherT

-- | Test the command `horse status` when run without a repository
testNoRepoStatus :: Assertion
testNoRepoStatus = testNoRepo $ H.status

-- | Test the command `horse stage` when run without a repository
testNoRepoStage :: Assertion
testNoRepoStage = testNoRepo $ H.stage Default.def

-- | Test the command `horse checkout` when run without a repository
testNoRepoCheckout :: Assertion
testNoRepoCheckout = testNoRepo $ H.checkout Default.def

-- | Test the command `horse commit` when run without a repository
testNoRepoCommit :: Assertion
testNoRepoCommit = testNoRepo $ quietCommit Default.def

-- | Test the command `horse show` when run without a repository
testNoRepoShow :: Assertion
testNoRepoShow = testNoRepo $ H.hshow Default.def

-- | Test the command `horse log` when run without a repository
testNoRepoLog :: Assertion
testNoRepoLog = testNoRepo $ H.log Default.def Default.def

-- | Tests diffing. Assumes working `horse commit`
testCheckout :: Assertion
testCheckout = do
    H.init (Just Quiet)

    let addedFile = "a"
    handle <- IO.openFile addedFile IO.WriteMode
    IO.hPutStr handle "a"
    IO.hClose handle

    runEitherT $ H.stage addedFile
    return ()

tests :: TestTree
tests = testGroup "unit tests"
    [ testCase
        "Testing `horse init`"
        (runTest testInit) 
    , testCase
        "Testing `horse stage`"
        (runTest testStage) 
    , testCase
        "Testing `horse log`"
        (runTest testLog) 
    , testCase
        "Testing command `status` run without a repo"
        (runTest testNoRepoStatus)
    , testCase
        "Testing command `stage` run without a repo"
        (runTest testNoRepoStage)
    , testCase
        "Testing command `checkout` run without a repo"
        (runTest testNoRepoCheckout)
    , testCase
        "Testing command `commit` run without a repo"
        (runTest testNoRepoCommit)
    , testCase
        "Testing command `show` run without a repo"
        (runTest testNoRepoShow)
    , testCase
        "Testing command `log` run without a repo"
        (runTest testNoRepoLog)
    , testCase
        "Testing diffs being stored in commits"
        (runTest testCheckout) ]

main :: IO ()
main = defaultMain tests
