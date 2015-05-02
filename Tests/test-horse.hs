{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- imports

import Test.Tasty
import Test.Tasty.HUnit

import Data.Monoid
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

-- qualified imports

import qualified System.IO as IO
import qualified System.Directory as D

import qualified Data.Hex as Hex
import qualified Data.Default as Default
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString (c2w, w2c)

import qualified Crypto.Hash.SHA256 as SHA256

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

-- imported functions

import Data.List (sort)

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
import qualified Horse.Commands as H
import qualified Horse.Constants as H

import qualified Filediff as FD
import qualified Filediff.Types as FD

-- | Runs a test in its own empty directory.
-- | Effectively, it isolates it from all other tests.
runTest :: Assertion -> Assertion
runTest t = do
    testDirectory <- getTestDirectory
    D.createDirectory testDirectory
    D.setCurrentDirectory testDirectory

    t

    D.setCurrentDirectory ".."
    D.removeDirectoryRecursive testDirectory
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
quietCommit m = H.commit Default.def m (Just Quiet)

noargCommit :: EitherT Error IO Commit
noargCommit = H.commit Default.def Nothing (Just Quiet)

getStatus :: IO Status
getStatus = do
    eitherStatus <- runEitherT $ H.status (Just Quiet)
    assertBool "`status` should not fail" (isRight eitherStatus)
    return $ fromRight undefined eitherStatus


assertFieldEqual :: (Eq a, Show a) => Commit -> Commit -> (Commit -> a) -> Assertion
assertFieldEqual a b field = field a @?= field b

testLog :: Assertion
testLog = do
    runEitherT $ H.init (Just Quiet)
    eitherSuccess <- runEitherT $ do
        liftIO $ createFileWithContents "a" "a"
        H.stage "a"
        commitA <- noargCommit

        liftIO $ createFileWithContents "b" "b"
        H.stage "b"
        commitB <- noargCommit

        liftIO $ createFileWithContents "c" "c"
        H.stage "c"
        commitC <- noargCommit

        liftIO $ createFileWithContents "d" "d"
        H.stage "d"
        commitD <- noargCommit

        let commits = [commitA, commitB, commitC, commitD]

        history <- reverse <$> H.log Nothing Nothing (Just Quiet)
        liftIO $ commits @?= history
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testLogEdgeCase1 :: Assertion
testLogEdgeCase1 = do
    runEitherT $ H.init (Just Quiet)
    eitherSuccess <- runEitherT $ do
        history <- reverse <$> H.log Nothing Nothing (Just Quiet)

        liftIO $ [] @?= history

    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testLogEdgeCase2 :: Assertion
testLogEdgeCase2 = do
    runEitherT $ H.init (Just Quiet)
    eitherSuccess <- runEitherT $ do
        liftIO $ createFileWithContents "a" "a"
        H.stage "a"
        commitA <- noargCommit

        liftIO $ createFileWithContents "b" "b"
        H.stage "b"
        commitB <- noargCommit

        liftIO $ createFileWithContents "c" "c"
        H.stage "c"
        commitC <- noargCommit

        liftIO $ createFileWithContents "d" "d"
        H.stage "d"
        commitD <- noargCommit

        let commits = [commitA, commitB, commitC, commitD]

        let ref = H.hashToString . hash $ commits !! 2
        history <- reverse <$> H.log (Just ref) Nothing (Just Quiet)
        liftIO $ (take (2+1) commits) @?= history
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testLogEdgeCase3 :: Assertion
testLogEdgeCase3 = do
    runEitherT $ H.init (Just Quiet)
    eitherSuccess <- runEitherT $ do
        liftIO $ createFileWithContents "a" "a"
        H.stage "a"
        commitA <- noargCommit

        liftIO $ createFileWithContents "b" "b"
        H.stage "b"
        commitB <- noargCommit

        liftIO $ createFileWithContents "c" "c"
        H.stage "c"
        commitC <- noargCommit

        liftIO $ createFileWithContents "d" "d"
        H.stage "d"
        commitD <- noargCommit

        let commits = [commitA, commitB, commitC, commitD]

        let ref = H.hashToString . hash $ head commits
        history <- reverse <$> H.log (Just ref) Nothing (Just Quiet)
        liftIO $ ([head commits]) @?= history
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testStage :: Assertion
testStage = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "a"

    runEitherT $ H.stage "a"

    eitherStagingArea <- runEitherT H.loadStagingArea

    (Right ["a"]) @?= (adds <$> eitherStagingArea)
    (Right [])    @?= (mods <$> eitherStagingArea)
    (Right [])    @?= (dels <$> eitherStagingArea)

testStageDirectory :: Assertion
testStageDirectory = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "dir"
    D.createDirectory "dir/sd"
    createFileWithContents "dir/sd/a" "a"

    runEitherT $ H.stage "dir/sd/a"
    runEitherT noargCommit

    createFileWithContents "dir/sd/b" "b"
    createFileWithContents "dir/sd/c" "c"

    runEitherT $ H.stage "dir/sd"

    status <- getStatus
    let stagedFiles = (stagingArea status) { adds = sort . adds $ stagingArea status }
    stagedFiles @?= StagingArea (sort ["dir/sd/b", "dir/sd/c"]) [] []
    unstagedFiles status @?= []

testStageDirectoryEdgeCase1 :: Assertion
testStageDirectoryEdgeCase1 = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "dir"
    D.createDirectory "dir/sd"
    createFileWithContents "dir/sd/a" "a"

    runEitherT $ H.stage "dir/sd/a"
    runEitherT noargCommit

    createFileWithContents "dir/sd/b" "b"
    createFileWithContents "dir/sd/c" "c"

    D.setCurrentDirectory "dir"
    runEitherT $ H.stage "sd"

    status <- getStatus
    let stagedFiles = (stagingArea status) { adds = sort . adds $ stagingArea status }
    stagedFiles @?= StagingArea (sort ["dir/sd/b", "dir/sd/c"]) [] []
    unstagedFiles status @?= []

    D.setCurrentDirectory ".."

testStageDirectoryEdgeCase2 :: Assertion
testStageDirectoryEdgeCase2 = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "dir"
    D.createDirectory "dir/sd"
    createFileWithContents "dir/sd/a" "a"

    runEitherT $ H.stage "dir/sd/a"
    runEitherT noargCommit

    createFileWithContents "dir/sd/b" "b"
    createFileWithContents "dir/sd/c" "c"

    D.setCurrentDirectory "dir"
    runEitherT $ H.stage "."

    status <- getStatus
    let stagedFiles = (stagingArea status) { adds = sort . adds $ stagingArea status }
    stagedFiles @?= StagingArea (sort ["dir/sd/b", "dir/sd/c"]) [] []
    unstagedFiles status @?= []

    D.setCurrentDirectory ".."

testStageDirectoryEdgeCase3 :: Assertion
testStageDirectoryEdgeCase3 = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "dir"
    D.createDirectory "dir/sd"
    createFileWithContents "dir/sd/a" "a"

    runEitherT $ H.stage "dir/sd/a"
    runEitherT noargCommit

    createFileWithContents "dir/sd/b" "b"
    createFileWithContents "dir/sd/c" "c"
    createFileWithContents "x" "x"

    D.setCurrentDirectory "dir"
    runEitherT $ H.stage "../x"

    status <- getStatus
    let stagedFiles = (stagingArea status) { adds = sort . adds $ stagingArea status }
    stagedFiles @?= StagingArea ["x"] [] []
    (sort $ unstagedFiles status) @?= (sort ["dir/sd/b", "dir/sd/c"])

    D.setCurrentDirectory ".."

testStageDirectoryEdgeCase4 :: Assertion
testStageDirectoryEdgeCase4 = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "a"

    runEitherT $ H.stage "."

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= Right (Status (StagingArea ["a"] [] []) [])

testStageDirectoryEdgeCase5 :: Assertion
testStageDirectoryEdgeCase5 = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "a"

    runEitherT $ H.stage "./"

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= Right (Status (StagingArea ["a"] [] []) [])

testStageDirectoryEdgeCase6 :: Assertion
testStageDirectoryEdgeCase6 = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "dir"
    createFileWithContents "a" "a"

    runEitherT $ H.stage "dir/.."

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= Right (Status (StagingArea ["a"] [] []) [])

testStageNonexistentFile :: Assertion
testStageNonexistentFile = do
    runEitherT $ H.init (Just Quiet)

    eitherStagingArea <- runEitherT $ H.stage "xyz"

    assertBool "Shouldn't stage a deletion of a nonexistent file." (isLeft eitherStagingArea)

testStageNonexistentDirectory :: Assertion
testStageNonexistentDirectory = do
    runEitherT $ H.init (Just Quiet)

    eitherStagingArea <- runEitherT $ H.stage "xyz"

    assertBool "Shouldn't stage a deletion of a nonexistent directory." (isLeft eitherStagingArea)

testStagePathOutsideOfRepo :: Assertion
testStagePathOutsideOfRepo = do
    D.createDirectory "repo"
    createFileWithContents "a" "a"
    D.setCurrentDirectory "repo"

    runEitherT $ H.init (Just Quiet)

    eitherStagingArea <- runEitherT $ H.stage "../a"

    D.setCurrentDirectory ".."

    eitherStagingArea @?= Left "Can't stage file or directory outside of the repository: ../a"

testStatusCase1 :: Assertion
testStatusCase1 = do
    runEitherT $ H.init (Just Quiet)

    eitherStatus <- runEitherT $ H.status (Just Quiet)

    assertBool "`status` command should not fail" (isRight eitherStatus)
    stagingArea <$> eitherStatus @?= Right Default.def

testStatusCase2 :: Assertion
testStatusCase2 = do
    runEitherT $ H.init (Just Quiet)

    eitherStatus <- runEitherT $ H.status (Just Quiet)

    assertBool "`status` command should not fail" (isRight eitherStatus)
    unstagedFiles <$> eitherStatus @?= Right Default.def

testStatusCase3 :: Assertion
testStatusCase3 = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "a"
    eitherStatus <- runEitherT $ H.status (Just Quiet)

    assertBool "`status` command should not fail" (isRight eitherStatus)
    let status = fromRight undefined eitherStatus

    stagingArea status @?= Default.def
    unstagedFiles status @?= ["a"]
    return ()

testStatusCase4 :: Assertion
testStatusCase4 = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"

    runEitherT noargCommit

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    assertBool "`status` command should not fail" (isRight eitherStatus)
    let status = fromRight undefined eitherStatus
    status @?= Default.def

    createFileWithContents "b" "b"
    runEitherT $ H.stage "b"
    runEitherT noargCommit

    appendFile "a" "aaaa"

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    assertBool "`status` command should not fail" (isRight eitherStatus)
    let status = fromRight undefined eitherStatus
    status @?= Status (StagingArea [] [] []) ["a"]

    return ()

testStatusCase5 :: Assertion
testStatusCase5 = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    appendFile "a" "bcd"
    runEitherT $ H.stage "a"
    eitherStatus <- runEitherT $ H.status (Just Quiet)
    assertBool "`status` command should not fail" (isRight eitherStatus)
    let status = fromRight undefined eitherStatus
    status @?= Status (StagingArea [] ["a"] []) []

    return ()

testInit :: Assertion
testInit = do
    runEitherT $ H.init (Just Quiet)
    rootDirectoryCreated <- D.doesDirectoryExist H.repositoryDataDir
    rootDirectoryCreated @?= True

testInitTwiceInSameDirectory :: Assertion
testInitTwiceInSameDirectory = do
    eitherInit1 <- runEitherT $ H.init (Just Quiet)
    eitherInit2 <- runEitherT $ H.init (Just Quiet)

    assertBool (fromLeft undefined eitherInit1) (isRight eitherInit1)
    assertBool "Error: command should fail" (isLeft eitherInit2)

testInitAgainInSubdir :: Assertion
testInitAgainInSubdir = do
    eitherInit1 <- runEitherT $ H.init (Just Quiet)
    D.createDirectory "x"
    D.setCurrentDirectory "x"
    eitherInit2 <- runEitherT $ H.init (Just Quiet)

    assertBool (fromLeft undefined eitherInit1) (isRight eitherInit1)
    assertBool "Error: command should fail" (isLeft eitherInit2)

    D.setCurrentDirectory ".."

testNoRepo :: EitherT Error IO a -> Assertion
testNoRepo = (=<<) ((@?=) True . isLeft) . runEitherT

testNoRepoStatus :: Assertion
testNoRepoStatus = testNoRepo $ H.status (Just Quiet)

testNoRepoStage :: Assertion
testNoRepoStage = testNoRepo $ H.stage Default.def

testNoRepoCheckout :: Assertion
testNoRepoCheckout = testNoRepo $ H.checkout Default.def (Just Quiet)

testNoRepoCommit :: Assertion
testNoRepoCommit = testNoRepo $ noargCommit

testNoRepoShow :: Assertion
testNoRepoShow = testNoRepo $ H.show Default.def (Just Quiet)

testNoRepoLog :: Assertion
testNoRepoLog = testNoRepo $ H.log Default.def Default.def Default.def

testNoRepoSquash :: Assertion
testNoRepoSquash = testNoRepo $ H.squash Default.def Default.def

testNoRepoUnstage :: Assertion
testNoRepoUnstage = testNoRepo $ H.unstage Default.def

testNoRepoIgnore :: Assertion
testNoRepoIgnore = testNoRepo $ H.ignore Default.def

testNoRepoUnignore :: Assertion
testNoRepoUnignore = testNoRepo $ H.unignore Default.def

testNoRepoListIgnored :: Assertion
testNoRepoListIgnored = testNoRepo $ H.listIgnoredPaths (Just Quiet)

testCheckoutChangesHEAD :: Assertion
testCheckoutChangesHEAD = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherFirstCommit <- runEitherT noargCommit

    createFileWithContents "b" "2"
    runEitherT $ H.stage "b"
    eitherSecondCommit <- runEitherT noargCommit

    history1 <- runEitherT $ H.log Nothing Nothing (Just Quiet)
    length <$> history1 @?= Right 2

    let firstHash = hash $ fromRight undefined eitherFirstCommit
    runEitherT $ H.checkout (H.hashToString firstHash) (Just Quiet)
    history2 <- runEitherT $ H.log Nothing Nothing (Just Quiet)
    length <$> history2 @?= Right 1

testCheckout :: Assertion
testCheckout = do

    runEitherT $ H.init (Just Quiet)

    ----------------

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let first = hash $ fromRight undefined eitherCommit1

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let second = hash $ fromRight undefined eitherCommit2

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "3"
    D.removeFile "b"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit3 <- runEitherT noargCommit
    let third = hash $ fromRight undefined eitherCommit3

    ----------------

    -- try multiple combinations of gaps and orders and such
    test1 first
    test2 second
    test3 third
    test2 second
    test1 first
    test3 third
    test1 first

    return ()
    where   
        test1 :: Hash -> Assertion
        test1 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "1"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

        test2 :: Hash -> Assertion
        test2 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "2"

            aContents <- readFile "b"
            aContents @?= "2"
            return ()

        test3 :: Hash -> Assertion
        test3 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "3"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

quietCheckout :: String -> IO ()
quietCheckout ref = fromRight undefined <$> (runEitherT $ H.checkout ref (Just Quiet))

testStatusFromSubdir :: Assertion
testStatusFromSubdir = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "a"
    D.createDirectory "a/b"
    D.createDirectory "a/b/c"
    createFileWithContents "a/b/file" "x"
    D.setCurrentDirectory "a/b/c"

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    assertBool "`status` command should not fail" (isRight eitherStatus)
    let status = fromRight undefined eitherStatus
    status @?= Status (StagingArea [] [] []) ["a/b/file"]

    D.setCurrentDirectory "../../.."

testStageFromSubdir :: Assertion
testStageFromSubdir = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "a"
    createFileWithContents "a/x" "x"
    D.setCurrentDirectory "a"

    runEitherT $ H.stage "x"

    -- test that current directory did not change
    currDirContents <- D.getDirectoryContents "."
    (sort currDirContents) @?= (sort [".", "..", "x"])
    D.setCurrentDirectory ".."

    eitherStagingArea <- runEitherT H.loadStagingArea
    eitherStagingArea @?= Right (StagingArea ["a/x"] [] [])

testCheckoutFromSubdir :: Assertion
testCheckoutFromSubdir = do
    runEitherT $ H.init (Just Quiet)

    ----------------

    D.createDirectory "dir"
    D.setCurrentDirectory "dir"

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight undefined eitherCommit1

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "3"
    D.removeFile "b"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit3 <- runEitherT noargCommit
    let hash3 = hash $ fromRight undefined eitherCommit3

    ----------------

    -- try multiple combinations of gaps and orders and such
    test1 hash1
    test2 hash2
    test3 hash3
    test2 hash2
    test1 hash1
    test3 hash3
    test1 hash1

    D.setCurrentDirectory ".."

    return ()
    where   
        test1 :: Hash -> Assertion
        test1 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "1"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

        test2 :: Hash -> Assertion
        test2 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "2"

            aContents <- readFile "b"
            aContents @?= "2"
            return ()

        test3 :: Hash -> Assertion
        test3 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "3"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

testCommitFromSubdir :: Assertion
testCommitFromSubdir = do
    runEitherT $ H.init (Just Quiet)

    ----------------

    D.createDirectory "dir"
    D.setCurrentDirectory "dir"
    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight undefined eitherCommit1
    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2
    ----------------

    D.removeFile "a" >> createFileWithContents "a" "3"
    D.removeFile "b"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit3 <- runEitherT noargCommit
    let hash3 = hash $ fromRight undefined eitherCommit3

    ----------------

    -- try multiple combinations of gaps and orders and such
    test1 hash1
    test2 hash2
    test3 hash3
    test2 hash2
    test1 hash1
    test3 hash3
    test1 hash1

    D.setCurrentDirectory ".."

    return ()
    where   
        test1 :: Hash -> Assertion
        test1 hash = do
            quietCheckout . H.hashToString $ hash

            c <- D.getDirectoryContents "."

            aContents <- readFile "a"
            aContents @?= "1"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."

        test2 :: Hash -> Assertion
        test2 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "2"

            aContents <- readFile "b"
            aContents @?= "2"

        test3 :: Hash -> Assertion
        test3 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "3"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."

        quietCheckout :: String -> IO ()
        quietCheckout ref = fromRight undefined <$> (runEitherT $ H.checkout ref (Just Quiet))

testShowFromSubdir :: Assertion
testShowFromSubdir = do
    runEitherT $ H.init (Just Quiet)

    ----------------

    D.createDirectory "dir"
    D.setCurrentDirectory "dir"
    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit <- runEitherT noargCommit

    assertBool "`commit` should not fail." $ isRight eitherCommit
    let commit = fromRight undefined eitherCommit

    eitherShownCommit <- runEitherT $ H.show (Just $ H.hashToString $ hash commit) (Just Quiet)
    assertBool "`commit` should not fail." $ isRight eitherShownCommit
    let shownCommit = fromRight undefined eitherShownCommit

    shownCommit @?= commit

    D.setCurrentDirectory ".."

testLogFromSubdir :: Assertion
testLogFromSubdir = do
    runEitherT $ H.init (Just Quiet)
    eitherSuccess <- runEitherT $ do
        liftIO $ D.createDirectory "dir"
        liftIO $ D.setCurrentDirectory "dir"

        liftIO $ createFileWithContents "a" "a"
        H.stage "a"
        commitA <- noargCommit

        liftIO $ createFileWithContents "b" "b"
        H.stage "b"
        commitB <- noargCommit

        liftIO $ createFileWithContents "c" "c"
        H.stage "c"
        commitC <- noargCommit

        liftIO $ createFileWithContents "d" "d"
        H.stage "d"
        commitD <- noargCommit

        let commits = [commitA, commitB, commitC, commitD]

        history <- reverse <$> H.log Nothing Nothing (Just Quiet)
        liftIO $ commits @?= history
    D.setCurrentDirectory ".."
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testCommitAmend :: Assertion
testCommitAmend = do
    runEitherT $ H.init (Just Quiet)

    ----------------

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherFirstCommit <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherFirstCommit) (isRight eitherFirstCommit)
    let firstCommit = fromRight undefined eitherFirstCommit

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "3"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    runEitherT $ H.commitAmend Default.def Nothing (Just Quiet)

    ----------------

    eitherLog <- runEitherT $ H.log Nothing Nothing (Just Quiet)
    assertBool ("`log` should not fail: " ++ (fromLeft undefined eitherLog))  (isRight eitherLog)
    let log = fromRight undefined eitherLog

    length log @?= 1
    let squashedCommit = head log

    assertFieldEqual firstCommit squashedCommit author
    assertFieldEqual firstCommit squashedCommit date
    assertFieldEqual firstCommit squashedCommit parentHash

    diffWithPrimaryParent squashedCommit @?= FD.Diff
        { FD.filediffs =
            [ FD.Filediff
                { FD.base = "b"
                , FD.comp = "b"
                , FD.change = FD.Add $ FD.ListDiff
                    { FD.dels = []
                    , FD.adds = [(0,"3")]
                    }
                }
            , FD.Filediff
                { FD.base = "a"
                , FD.comp = "a"
                , FD.change = FD.Add $ FD.ListDiff
                    { FD.dels = []
                    , FD.adds = [(0,"2")]
                    }
                }
            ]
        }

    -- TODO: make this better
    message squashedCommit @?= message firstCommit <> "default message"

    assertBool "Hashes should not be equal."
        (hash squashedCommit /= hash firstCommit)

testCommitAmendFromSubdir :: Assertion
testCommitAmendFromSubdir = do
    runEitherT $ H.init (Just Quiet)

    ----------------

    D.createDirectory "d"
    D.setCurrentDirectory "d"
    createFileWithContents "a" "1"

    runEitherT $ H.stage "."
    eitherFirstCommit <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherFirstCommit) (isRight eitherFirstCommit)
    let firstCommit = fromRight undefined eitherFirstCommit

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "3"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    runEitherT $ H.commitAmend Default.def Nothing (Just Quiet)

    ----------------

    eitherLog <- runEitherT $ H.log Nothing Nothing (Just Quiet)
    assertBool ("`log` should not fail: " ++ (fromLeft undefined eitherLog))  (isRight eitherLog)
    let log = fromRight undefined eitherLog

    length log @?= 1
    let squashedCommit = head log

    assertFieldEqual firstCommit squashedCommit author
    assertFieldEqual firstCommit squashedCommit date
    assertFieldEqual firstCommit squashedCommit parentHash

    diffWithPrimaryParent squashedCommit @?= FD.Diff
        { FD.filediffs =
            [ FD.Filediff
                { FD.base = "d/b"
                , FD.comp = "d/b"
                , FD.change = FD.Add $ FD.ListDiff
                    { FD.dels = []
                    , FD.adds = [(0,"3")]
                    }
                }
            , FD.Filediff
                { FD.base = "d/a"
                , FD.comp = "d/a"
                , FD.change = FD.Add $ FD.ListDiff
                    { FD.dels = []
                    , FD.adds = [(0,"2")]
                    }
                }
            ]
        }

    -- TODO: make this better
    message squashedCommit @?= message firstCommit <> "default message"

    assertBool "Hashes should not be equal."
        (hash squashedCommit /= hash firstCommit)

    D.setCurrentDirectory ".."

testSquash :: Assertion
testSquash = do
    runEitherT $ H.init (Just Quiet)

    ----------------

    createFileWithContents "a" "1"

    runEitherT $ H.stage "."
    eitherFirstCommit <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherFirstCommit) (isRight eitherFirstCommit)
    let firstCommit = fromRight undefined eitherFirstCommit
 
    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "."
    runEitherT noargCommit

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "3"
    D.removeFile "b" >> createFileWithContents "b" "3"
    createFileWithContents "c" "3"

    runEitherT $ H.stage "."
    runEitherT noargCommit

    ----------------

    runEitherT $ H.squash Default.def (H.hashToString . hash $ firstCommit)

    eitherLog <- runEitherT $ H.log Nothing Nothing (Just Quiet)
    assertBool ("`log` should not fail: " ++ (fromLeft undefined eitherLog))  (isRight eitherLog)
    let log = fromRight undefined eitherLog

    length log @?= 1
    let squashedCommit = head log

    assertFieldEqual firstCommit squashedCommit author
    assertFieldEqual firstCommit squashedCommit date
    assertFieldEqual firstCommit squashedCommit parentHash

    diffWithPrimaryParent squashedCommit @?= FD.Diff
        { FD.filediffs =
            [ FD.Filediff
                { FD.base = "c"
                , FD.comp = "c"
                , FD.change = FD.Add $ FD.ListDiff
                    { FD.dels = []
                    , FD.adds = [(0,"3")]
                    }
                }
            , FD.Filediff
                { FD.base = "b"
                , FD.comp = "b"
                , FD.change = FD.Add $ FD.ListDiff
                    { FD.dels = []
                    , FD.adds = [(0,"3")]
                    }
                }
            , FD.Filediff
                { FD.base = "a"
                , FD.comp = "a"
                , FD.change = FD.Add $ FD.ListDiff
                    { FD.dels = []
                    , FD.adds = [(0,"3")]
                    }
                }
            ]
        }

    -- TODO: make this better
    message squashedCommit @?= message firstCommit <> "default message" <> "default message"

    assertBool "Hashes should not be equal."
        (hash squashedCommit /= hash firstCommit)

testSquashFromSubdir :: Assertion
testSquashFromSubdir = do
    runEitherT $ H.init (Just Quiet)

    ----------------

    D.createDirectory "d"
    D.setCurrentDirectory "d"

    createFileWithContents "a" "1"

    runEitherT $ H.stage "."
    eitherFirstCommit <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherFirstCommit) (isRight eitherFirstCommit)
    let firstCommit = fromRight undefined eitherFirstCommit
 
    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "."
    runEitherT noargCommit

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "3"
    D.removeFile "b" >> createFileWithContents "b" "3"
    createFileWithContents "c" "3"

    runEitherT $ H.stage "."
    runEitherT noargCommit

    ----------------

    runEitherT $ H.squash Default.def (H.hashToString . hash $ firstCommit)

    eitherLog <- runEitherT $ H.log Nothing Nothing (Just Quiet)
    assertBool ("`log` should not fail: " ++ (fromLeft undefined eitherLog))  (isRight eitherLog)
    let log = fromRight undefined eitherLog

    length log @?= 1
    let squashedCommit = head log

    assertFieldEqual firstCommit squashedCommit author
    assertFieldEqual firstCommit squashedCommit date
    assertFieldEqual firstCommit squashedCommit parentHash

    diffWithPrimaryParent squashedCommit @?= FD.Diff
        { FD.filediffs =
            [ FD.Filediff
                { FD.base = "d/c"
                , FD.comp = "d/c"
                , FD.change = FD.Add $ FD.ListDiff
                    { FD.dels = []
                    , FD.adds = [(0,"3")]
                    }
                }
            , FD.Filediff
                { FD.base = "d/b"
                , FD.comp = "d/b"
                , FD.change = FD.Add $ FD.ListDiff
                    { FD.dels = []
                    , FD.adds = [(0,"3")]
                    }
                }
            , FD.Filediff
                { FD.base = "d/a"
                , FD.comp = "d/a"
                , FD.change = FD.Add $ FD.ListDiff
                    { FD.dels = []
                    , FD.adds = [(0,"3")]
                    }
                }
            ]
        }

    -- TODO: make this better
    message squashedCommit @?= message firstCommit <> "default message" <> "default message"

    assertBool "Hashes should not be equal."
        (hash squashedCommit /= hash firstCommit)

    D.setCurrentDirectory ".."

testStageSameFileTwiceNoChanges :: Assertion
testStageSameFileTwiceNoChanges = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "a"

    status <- getStatus
    status @?= Status (StagingArea ["a"] [] []) []

testStageSameFileTwiceWithChanges :: Assertion
testStageSameFileTwiceWithChanges = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"

    appendFile "a" "2"

    runEitherT $ H.stage "a"

    status <- getStatus
    status @?= Status (StagingArea ["a"] [] []) []

    return ()

testUnstage :: Assertion
testUnstage = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"

    firstStatus <- getStatus
    firstStatus @?= Status (StagingArea ["a"] [] []) []

    runEitherT $ H.unstage "a"

    secondStatus <- getStatus
    secondStatus @?= Status (StagingArea [] [] []) ["a"]

testUnstageUnstagedFile :: Assertion
testUnstageUnstagedFile = do
    runEitherT $ H.init (Just Quiet)

    runEitherT $ H.stage "a"
    runEitherT $ H.unstage "a"

    status <- getStatus
    status @?= Status (StagingArea [] [] []) []

testUnstageNonexistentPath :: Assertion
testUnstageNonexistentPath = do
    runEitherT $ H.init (Just Quiet)

    runEitherT $ H.stage "a"
    runEitherT $ H.unstage "a"

    status <- getStatus
    status @?= Status (StagingArea [] [] []) []

testUnstageFromSubdir :: Assertion
testUnstageFromSubdir = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "d"
    D.setCurrentDirectory "d"

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    runEitherT $ H.unstage "a"

    status <- getStatus
    status @?= Status (StagingArea [] [] []) ["d/a"]

    D.setCurrentDirectory ".."

testUnstageDirectory :: Assertion
testUnstageDirectory = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "d"

    createFileWithContents "d/a" "1"
    createFileWithContents "d/b" "1"

    runEitherT $ H.stage "d"
    runEitherT $ H.unstage "d"

    status <- getStatus
    stagingArea status @?= StagingArea [] [] []
    (sort $ unstagedFiles status) @?= (sort ["d/a", "d/b"])

testCommitNoStagedFiles :: Assertion
testCommitNoStagedFiles = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "1"

    eitherCommit <- runEitherT noargCommit

    assertBool "Error: shouldn't be able to commit with empty staging area." (isLeft eitherCommit)

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= (Right $ Status (StagingArea [] [] []) ["a"])

testStageFileWithNoChanges :: Assertion
testStageFileWithNoChanges = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= (Right $ Status (StagingArea [] [] []) [])

    eitherStagingArea <- runEitherT $ H.stage "a"
    eitherStagingArea @?= (Right $ StagingArea [] [] [])

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= (Right $ Status (StagingArea [] [] []) [])

testCheckoutTruncatedHash :: Assertion
testCheckoutTruncatedHash = do
    runEitherT $ H.init (Just Quiet)

    ----------------

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let first = ByteString.take 8 . hash $ fromRight undefined eitherCommit1

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let second = ByteString.take 8 . hash $ fromRight undefined eitherCommit2

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "3"
    D.removeFile "b"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit3 <- runEitherT noargCommit
    let third = ByteString.take 8 . hash $ fromRight undefined eitherCommit3

    ----------------

    -- try multiple combinations of gaps and orders and such
    test1 first
    test2 second
    test3 third
    test2 second
    test1 first
    test3 third
    test1 first

    return ()
    where   
        test1 :: Hash -> Assertion
        test1 hash = do
            x <- quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "1"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

        test2 :: Hash -> Assertion
        test2 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "2"

            aContents <- readFile "b"
            aContents @?= "2"
            return ()

        test3 :: Hash -> Assertion
        test3 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "3"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

testCheckoutBadTruncatedHash1 :: Assertion
testCheckoutBadTruncatedHash1 = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let commitHash = ByteString.take 8 . hash $ fromRight undefined eitherCommit1

    eitherCheckoutResult <- runEitherT $ H.checkout "" (Just Quiet)

    assertBool "Loading empty hash should fail." (isLeft eitherCheckoutResult)

testCheckoutBadTruncatedHash2 :: Assertion
testCheckoutBadTruncatedHash2 = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    runEitherT noargCommit

    -- unlikely that this will be the actual hash
    eitherCheckoutResult <- runEitherT $ H.checkout "aaaaaaaa" (Just Quiet)

    assertBool "Loading bad hash should fail." (isLeft eitherCheckoutResult)

-- always hashes to "aaaaaaaaa..." (40 'a's)
mockHasher1 :: CommitHasher
mockHasher1 = CommitHasher
    (const $ ByteString.pack . map ByteString.c2w $ replicate 40 'a')

-- always hashes to "aaaaaaaaabbbbbbbbbbb..." (9 'a's, then 31 'b's)
mockHasher2 :: CommitHasher
mockHasher2 = CommitHasher
    ( const
    $ ByteString.pack
    $ map ByteString.c2w
    $ (replicate 9 'a') ++ (replicate 31 'b') )

testCheckoutCollidingTruncatedHashes :: Assertion
testCheckoutCollidingTruncatedHashes = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    runEitherT $ H.commit mockHasher1 Nothing (Just Quiet)

    createFileWithContents "b" "1"
    runEitherT $ H.stage "b"
    runEitherT $ H.commit mockHasher2 Nothing (Just Quiet)

    eitherCheckoutSuccess <- runEitherT $ H.checkout (replicate 9 'a') (Just Quiet)

    assertBool "Committing with a colliding truncated hash should fail." (isLeft eitherCheckoutSuccess)

testCheckoutRelativeSyntaxCaret :: Assertion
testCheckoutRelativeSyntaxCaret = do
    runEitherT $ H.init (Just Quiet)

    ----------------

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "3"
    D.removeFile "b"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit3 <- runEitherT noargCommit

    let third = ByteString.take 8 . hash $ fromRight undefined eitherCommit3
    let second = third <> "^"
    let first = second <> "^"

    ----------------

    -- try multiple combinations of gaps and orders and such
    test1 first
    test2 second
    test3 third
    test2 second
    test1 first
    test3 third
    test1 first

    return ()
    where   
        test1 :: Hash -> Assertion
        test1 hash = do
            x <- quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "1"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

        test2 :: Hash -> Assertion
        test2 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "2"

            aContents <- readFile "b"
            aContents @?= "2"
            return ()

        test3 :: Hash -> Assertion
        test3 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "3"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

testCheckoutRelativeSyntaxTilde :: Assertion
testCheckoutRelativeSyntaxTilde = do
    runEitherT $ H.init (Just Quiet)

    ----------------

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "3"
    D.removeFile "b"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit3 <- runEitherT noargCommit

    let third = hash $ fromRight undefined eitherCommit3
    let second = third <> "~1"
    let first = third <> "~2"

    ----------------

    -- try multiple combinations of gaps and orders and such
    test1 first
    test2 second
    test3 third
    test2 second
    test1 first
    test3 third
    test1 first

    return ()
    where   
        test1 :: Hash -> Assertion
        test1 hash = do
            x <- quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "1"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

        test2 :: Hash -> Assertion
        test2 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "2"

            aContents <- readFile "b"
            aContents @?= "2"
            return ()

        test3 :: Hash -> Assertion
        test3 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "3"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

testCheckoutTruncatedRelativeSyntax :: Assertion
testCheckoutTruncatedRelativeSyntax = do
    runEitherT $ H.init (Just Quiet)

    ----------------

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "3"
    D.removeFile "b"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit3 <- runEitherT noargCommit

    let third = ByteString.take 8 . hash $ fromRight undefined eitherCommit3
    let second = third <> "~1"
    let first = third <> "~2"

    ----------------

    -- try multiple combinations of gaps and orders and such
    test1 first
    test2 second
    test3 third
    test2 second
    test1 first
    test3 third
    test1 first

    return ()
    where   
        test1 :: Hash -> Assertion
        test1 hash = do
            x <- quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "1"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

        test2 :: Hash -> Assertion
        test2 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "2"

            aContents <- readFile "b"
            aContents @?= "2"
            return ()

        test3 :: Hash -> Assertion
        test3 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "3"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()


testCheckoutRelativeSyntaxTildeZero :: Assertion
testCheckoutRelativeSyntaxTildeZero = do
    runEitherT $ H.init (Just Quiet)

    ----------------

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let first = (hash $ fromRight undefined eitherCommit1) <> "~0"
    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let second = (hash $ fromRight undefined eitherCommit2) <> "~0"
    ----------------

    D.removeFile "a" >> createFileWithContents "a" "3"
    D.removeFile "b"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit3 <- runEitherT noargCommit
    let third = (hash $ fromRight undefined eitherCommit3) <> "~0"

    ----------------

    -- try multiple combinations of gaps and orders and such
    test1 first
    test2 second
    test3 third
    test2 second
    test1 first
    test3 third
    test1 first

    return ()
    where   
        test1 :: Hash -> Assertion
        test1 hash = do
            x <- quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "1"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

        test2 :: Hash -> Assertion
        test2 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "2"

            aContents <- readFile "b"
            aContents @?= "2"
            return ()

        test3 :: Hash -> Assertion
        test3 hash = do
            quietCheckout . H.hashToString $ hash

            aContents <- readFile "a"
            aContents @?= "3"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

testIgnore :: Assertion
testIgnore = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.ignore "a"

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b"])

testIgnoreGivenDirectory :: Assertion
testIgnoreGivenDirectory = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "x"
    D.createDirectory "x/y"
    createFileWithContents "x/a" "1"
    createFileWithContents "x/y/a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.ignore "x"

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b"])

testIgnoreGivenDirectoryFromSubdir :: Assertion
testIgnoreGivenDirectoryFromSubdir = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "x"
    D.createDirectory "x/y"
    createFileWithContents "x/a" "1"
    createFileWithContents "x/y/a" "1"
    createFileWithContents "b" "1"

    D.setCurrentDirectory "x"
    runEitherT $ H.ignore "."
    D.setCurrentDirectory ".."

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b"])

testUnignore :: Assertion
testUnignore = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.ignore "a"
    runEitherT $ H.unignore "a"

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["a", "b"])

testUnignoreGivenDirectory :: Assertion
testUnignoreGivenDirectory = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "x"
    D.createDirectory "x/y"
    createFileWithContents "x/a" "1"
    createFileWithContents "x/y/a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.ignore "x"
    runEitherT $ H.unignore "x"

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b", "x/a", "x/y/a"])

testUnignoreGivenDirectoryFromSubdir :: Assertion
testUnignoreGivenDirectoryFromSubdir = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "x"
    D.createDirectory "x/y"
    createFileWithContents "x/a" "1"
    createFileWithContents "x/y/a" "1"
    createFileWithContents "b" "1"

    D.setCurrentDirectory "x"
    runEitherT $ H.ignore "."
    runEitherT $ H.unignore "."
    D.setCurrentDirectory ".."

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b", "x/a", "x/y/a"])

testStagingIgnoredFile :: Assertion
testStagingIgnoredFile = do
    runEitherT $ H.init (Just Quiet)

    createFileWithContents "a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.ignore "b"
    runEitherT $ H.stage "b"

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["a"])

testIgnoreMultipleTimes :: Assertion
testIgnoreMultipleTimes = do
    runEitherT $ H.init (Just Quiet)

    D.createDirectory "x"
    D.createDirectory "x/y"
    createFileWithContents "x/a" "1"
    createFileWithContents "x/y/a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.ignore "x/a"
    runEitherT $ H.ignore "x/y"

    eitherStatus <- runEitherT $ H.status (Just Quiet)
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b"])

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
        "Testing command `squash` run without a repo"
        (runTest testNoRepoSquash)
    , testCase
        "Testing command `ignore` run without a repo"
        (runTest testNoRepoIgnore)
    , testCase
        "Testing command `listIgnored` run without a repo"
        (runTest testNoRepoListIgnored)
    , testCase
        "Testing command `unignore` run without a repo"
        (runTest testNoRepoUnignore)
    , testCase
        "Testing `horse init`"
        (runTest testInit)
    , testCase
        "Testing `horse init` (edge case 1)"
        (runTest testInitTwiceInSameDirectory)
    , testCase
        "Testing `horse init` (edge case 2)"
        (runTest testInitAgainInSubdir)
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
        "Testing command `stage`"
        (runTest testStage)
    , testCase
        "Testing command `stage` (edge case 1)"
        (runTest testStagePathOutsideOfRepo)
     , testCase
         "Testing command `stage` (edge case 1)"
         (runTest testStageNonexistentFile)
     , testCase
         "Testing command `stage` (edge case 2)"
         (runTest testStageNonexistentDirectory)
    , testCase
        "Testing command `stage` when given a directory"
        (runTest testStageDirectory)
    , testCase
        "Testing command `stage` when given a directory (edge case 1)"
        (runTest testStageDirectoryEdgeCase1)
    , testCase
        "Testing command `stage` when given a directory (edge case 2)"
        (runTest testStageDirectoryEdgeCase2)
    , testCase
        "Testing command `stage` when given a directory (edge case 3)"
        (runTest testStageDirectoryEdgeCase3)
    , testCase
        "Testing command `stage` when given a directory (edge case 4)"
        (runTest testStageDirectoryEdgeCase4)
    , testCase
        "Testing command `stage` when given a directory (edge case 5)"
        (runTest testStageDirectoryEdgeCase5)
    , testCase
        "Testing command `stage` when given a directory (edge case 6)"
        (runTest testStageDirectoryEdgeCase6)
    , testCase
        "Testing command `status` (case 1)"
        (runTest testStatusCase1)
    , testCase
        "Testing command `status` (case 2)"
        (runTest testStatusCase2)
    , testCase
        "Testing command `status` (case 3)"
        (runTest testStatusCase3)
    , testCase
        "Testing command `status` (case 4)"
        (runTest testStatusCase4)
    , testCase
        "Testing command `status` (case 5)"
        (runTest testStatusCase5)
    , testCase
        "Testing command `checkout`"
        (runTest testCheckout)
    , testCase
        "Testing executing command `status` from subdirectory"
        (runTest testStatusFromSubdir)
    , testCase
        "Testing executing command `stage` from subdirectory"
        (runTest testStageFromSubdir)
    , testCase
        "Testing executing command `checkout` from subdirectory"
        (runTest testCheckoutFromSubdir)
    , testCase
        "Testing executing command `commit` from subdirectory"
        (runTest testCommitFromSubdir)
    , testCase
        "Testing executing command `show` from subdirectory"
        (runTest testShowFromSubdir)
    , testCase
        "Testing executing command `log` from subdirectory"
        (runTest testLogFromSubdir)
    , testCase
        "Testing executing command `unstage` from a subdirectory"
        (runTest testUnstageFromSubdir)
    , testCase
        "Testing executing command `commit --amend`"
        (runTest testCommitAmend)
    , testCase
        "Testing executing command `commit --amend` from a subdirectory"
        (runTest testCommitAmendFromSubdir)
    , testCase
        "Testing executing command `squash`"
        (runTest testSquash)
    , testCase
        "Testing executing command `squash` from a subdirectory"
        (runTest testSquashFromSubdir)
    , testCase
        "Testing staging same file twice with no changes in between"
        (runTest testStageSameFileTwiceNoChanges)
    , testCase
        "Testing staging same file twice with changes in between"
        (runTest testStageSameFileTwiceWithChanges)
    , testCase
        "Testing command `unstage`"
        (runTest testUnstage)
    , testCase
        "Testing command `unstage` (edge case 1)"
        (runTest testUnstageNonexistentPath)
    , testCase
        "Testing command `unstage` (edge case 2)"
        (runTest testUnstageUnstagedFile)
    , testCase
        "Testing command `unstage` when given a directory"
        (runTest testUnstageDirectory)
    , testCase
        "Testing committing with no staged files"
        (runTest testCommitNoStagedFiles)
    , testCase
        "Testing staging a file with no changes"
        (runTest testStageFileWithNoChanges)
    , testCase
        "Testing command `checkout` changes HEAD"
        (runTest testCheckoutChangesHEAD)
    , testCase
        "Testing command `checkout` given with truncated hash"
        (runTest testCheckoutTruncatedHash)
    , testCase
        "Testing command `checkout` given a bad truncated hash (case 1)"
        (runTest testCheckoutBadTruncatedHash1)
    , testCase
        "Testing command `checkout` given a bad truncated hash (case 2)"
        (runTest testCheckoutBadTruncatedHash2)
    , testCase
        "Testing command `checkout` given colliding truncated hashes"
        (runTest testCheckoutCollidingTruncatedHashes)
    , testCase
        "Testing command `checkout` given relative hash (case 1)"
        (runTest testCheckoutRelativeSyntaxCaret)
    , testCase
        "Testing command `checkout` given relative hash (case 2)"
        (runTest testCheckoutRelativeSyntaxTilde)
    , testCase
        "Testing command `checkout` given truncated relative hash"
        (runTest testCheckoutTruncatedRelativeSyntax)
    , testCase
        "Testing command `checkout` given relative hash (edge case 1)"
        (runTest testCheckoutRelativeSyntaxTildeZero)
    , testCase
        "Testing command `ignore`"
        (runTest testIgnore)
    , testCase
        "Testing command `ignore` multiple times"
        (runTest testIgnoreMultipleTimes)
    , testCase
        "Testing command `ignore` (given a directory)"
        (runTest testIgnoreGivenDirectory)
    , testCase
        "Testing command `ignore` from a subdirectory"
        (runTest testIgnoreGivenDirectoryFromSubdir)
    , testCase
        "Testing command `ignore` by ignoring a file and then staging it"
        (runTest testStagingIgnoredFile)
    , testCase
        "Testing command `unignore`"
        (runTest testUnignore)
    , testCase
        "Testing command `unignore` (given a directory)"
        (runTest testUnignoreGivenDirectory)
    , testCase
        "Testing command `unignore` from a subdirectory"
        (runTest testUnignoreGivenDirectoryFromSubdir)
    ]

main :: IO ()
main = defaultMain tests
