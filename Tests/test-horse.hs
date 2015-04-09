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

import Data.Either.Combinators (isLeft, isRight, fromLeft, fromRight)

-- horse imports

import Horse.Types

import qualified Horse.IO as H
import qualified Horse.Utils as H
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

createFileWithContents :: FilePath -> String -> IO ()
createFileWithContents filepath contents = do
    handle <- IO.openFile filepath IO.WriteMode
    IO.hPutStr handle contents
    IO.hClose handle

quietCommit :: Maybe String -> EitherT Error IO Commit
quietCommit = (flip H.commit $ Just Quiet)

-- | Test the `horse log` command
testLog :: Assertion
testLog = do
    H.init (Just Quiet)
    eitherSuccess <- runEitherT $ do
        let messages = map Just ["A", "B", "C", "D"]
        commits <- mapM quietCommit messages

        history <- reverse <$> H.log Nothing Nothing (Just Quiet)
        liftIO $ commits @?= history
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft "" eitherSuccess)
    return ()

-- | Test the `horse log` command
testLogEdgeCase1 :: Assertion
testLogEdgeCase1 = do
    H.init (Just Quiet)
    eitherSuccess <- runEitherT $ do
        history <- reverse <$> H.log Nothing Nothing (Just Quiet)

        liftIO $ [] @?= history

    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft "" eitherSuccess)
    return ()

-- | Test the `horse log` command
testLogEdgeCase2 :: Assertion
testLogEdgeCase2 = do
    H.init (Just Quiet)
    eitherSuccess <- runEitherT $ do
        let messages = map Just ["A", "B", "C", "D"]
        commits <- mapM quietCommit messages

        let ref = H.hashToString . hash $ commits !! 2
        history <- reverse <$> H.log (Just ref) Nothing (Just Quiet)
        liftIO $ (take (2+1) commits) @?= history
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft "" eitherSuccess)
    return ()

-- | Test the `horse log` command
testLogEdgeCase3 :: Assertion
testLogEdgeCase3 = do
    H.init (Just Quiet)
    eitherSuccess <- runEitherT $ do
        let messages = map Just ["A", "B", "C", "D"]
        commits <- mapM quietCommit messages

        let ref = H.hashToString . hash $ head commits
        history <- reverse <$> H.log (Just ref) Nothing (Just Quiet)
        liftIO $ ([head commits]) @?= history
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft "" eitherSuccess)
    return ()

-- | Test the `horse stage` command
testStageCase1 :: Assertion
testStageCase1 = do
    H.init (Just Quiet)

    createFileWithContents "a" "a"

    runEitherT $ H.stage "a"

    eitherStagingArea <- runEitherT H.loadStagingArea

    (Right ["a"]) @?= (adds <$> eitherStagingArea)

    (Right [])    @?= (mods <$> eitherStagingArea)

    (Right [])    @?= (dels <$> eitherStagingArea)

-- | No files should result in an empty staging area
testStageCase2 :: Assertion
testStageCase2 = do
    H.init (Just Quiet)

    eitherStatus <- runEitherT $ H.status (Just Quiet)

    assertBool "`status` command should not fail" (isRight eitherStatus)
    stagingArea <$> eitherStatus @?= Right Default.def

-- | No files should result in no untracked files
testStageCase3 :: Assertion
testStageCase3 = do
    H.init (Just Quiet)

    eitherStatus <- runEitherT $ H.status (Just Quiet)

    assertBool "`status` command should not fail" (isRight eitherStatus)
    unstagedFiles <$> eitherStatus @?= Right Default.def

-- | Checks that we correctly identify all unstaged files
testStageCase4 :: Assertion
testStageCase4 = do
    H.init (Just Quiet)

    createFileWithContents "a" "a"
    eitherStatus <- runEitherT $ H.status (Just Quiet)

    assertBool "`status` command should not fail" (isRight eitherStatus)
    let status = fromRight undefined eitherStatus

    stagingArea status @?= Default.def
    unstagedFiles status @?= ["a"]
    return ()

-- | Test the `horse init` command
testInit :: Assertion
testInit = do
    H.init (Just Quiet)
    rootDirectoryCreated <- Dir.doesDirectoryExist H.repositoryDataDir
    rootDirectoryCreated @?= True

-- | Verify that a command failed gracefully, as it should when run
-- | without a repository in which to execute
testNoRepo :: EitherT Error IO a -> Assertion
testNoRepo = (=<<) ((@?=) True . isLeft) . runEitherT

-- | Test the command `horse status` when run without a repository
testNoRepoStatus :: Assertion
testNoRepoStatus = testNoRepo $ H.status (Just Quiet)

-- | Test the command `horse stage` when run without a repository
testNoRepoStage :: Assertion
testNoRepoStage = testNoRepo $ H.stage Default.def

-- | Test the command `horse checkout` when run without a repository
testNoRepoCheckout :: Assertion
testNoRepoCheckout = testNoRepo $ H.checkout Default.def (Just Quiet)

-- | Test the command `horse commit` when run without a repository
testNoRepoCommit :: Assertion
testNoRepoCommit = testNoRepo $ quietCommit Default.def

-- | Test the command `horse show` when run without a repository
testNoRepoShow :: Assertion
testNoRepoShow = testNoRepo $ H.hshow Default.def

-- | Test the command `horse log` when run without a repository
testNoRepoLog :: Assertion
testNoRepoLog = testNoRepo $ H.log Default.def Default.def Default.def

-- | Tests diffing. Assumes working `horse commit`
--testCheckout :: Assertion
--testCheckout = do 
    --H.init (Just Quiet)

    --createFileWithContents "a" "a"

    --runEitherT $ H.stage addedFile
    --return ()
    -- TODO

tests :: TestTree
tests = testGroup "unit tests"
    [ testCase
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
        "Testing `horse init`"
        (runTest testInit)
    , testCase
        "Testing `horse log`"
        (runTest testLog)
    , testCase
        "Testing `horse log` (edge case 1)"
        (runTest testLogEdgeCase1)
    , testCase
        "Testing `horse log` (edge case 2)"
        (runTest testLogEdgeCase2)
    , testCase
        "Testing `horse log` (edge case 3)"
        (runTest testLogEdgeCase3)
    , testCase
        "Testing command `stage` (case 1)"
        (runTest testStageCase1)
    , testCase
        "Testing command `stage` (case 2)"
        (runTest testStageCase2)
    , testCase
        "Testing command `stage` (case 3)"
        (runTest testStageCase3)
    , testCase
        "Testing command `stage` (case 4)"
        (runTest testStageCase4)
    --, testCase
    --    "Testing diffs being stored in commits"
    --    (runTest testCheckout)
    ]

main :: IO ()
main = defaultMain tests
