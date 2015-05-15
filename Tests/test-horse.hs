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
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString (c2w, w2c)

import qualified Crypto.Hash.SHA256 as SHA256

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

-- imported functions

import Data.Maybe

import Data.List (sort, find)

import Data.Default (def)

import System.Exit (exitSuccess)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class (liftIO)

import Data.Either.Combinators (isLeft, isRight, fromLeft, fromRight)

-- horse imports

import Horse.Types

import qualified Horse.IO as H
import qualified Horse.Utils as H
import qualified Horse.Commands as H
import qualified Horse.Constants as H
import qualified Horse.Filesystem as H

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
quietCommit m = H.commit def m quietPrinter

noargCommit :: EitherT Error IO Commit
noargCommit = H.commit def Nothing quietPrinter

getStatus :: IO Status
getStatus = do
    eitherStatus <- runEitherT $ H.status quietPrinter
    assertBool "`status` should not fail" (isRight eitherStatus)
    return $ fromRight undefined eitherStatus

assertFieldEqual :: (Eq a, Show a) => Commit -> Commit -> (Commit -> a) -> Assertion
assertFieldEqual a b field = field a @?= field b

testNoRepoRepoRoot :: Assertion
testNoRepoRepoRoot = do
    eitherRepoRoot <- runEitherT H.repoRoot
    eitherRepoRoot @?= Left "Fatal: current directory is not a repo or a decendant of one."

testRepoRootNoAncestors :: Assertion
testRepoRootNoAncestors = do
    originalDir <- D.getCurrentDirectory
    D.setCurrentDirectory "/"

    eitherRepoRoot <- runEitherT H.repoRoot
    eitherRepoRoot @?= Left "Fatal: current directory is not a repo or a decendant of one."

    D.setCurrentDirectory originalDir

testDestructivelyCreateDirectory :: Assertion
testDestructivelyCreateDirectory = do
    D.createDirectory "x"
    createFileWithContents "x/a" "a"
    H.destructivelyCreateDirectory "x"

    contents <- D.getDirectoryContents "."
    (sort contents) @?= (sort [".", "..", "x"])

testGetDirectoryContentsRecursiveSafe :: Assertion
testGetDirectoryContentsRecursiveSafe = do
    contents <- H.getDirectoryContentsRecursiveSafe "x"
    contents @?= []

testDropPrefix :: Assertion
testDropPrefix = do
    H.dropPrefix ("abc" :: String) ("ab"  :: String) @?= Nothing
    H.dropPrefix ("abc" :: String) ("abc" :: String) @?= Just []
    H.dropPrefix ("abc" :: String) ("axc" :: String) @?= Nothing

testDropUntil :: Assertion
testDropUntil = do
    H.dropUntil ((==) 'b') (""    :: String) @?= ""
    H.dropUntil ((==) 'b') ("abc" :: String) @?= "c"

testTakeWhileM :: Assertion
testTakeWhileM = do
    actual      <- H.takeWhileM (return . id) [True, True, False]
    let expected = takeWhile    (         id) [True, True, False]
    actual @?= expected

testTakeWhileMEdgeCase1 :: Assertion
testTakeWhileMEdgeCase1 = do
    actual      <- H.takeWhileM (return . id) []
    let expected = takeWhile    (         id) []
    actual @?= expected

testTakeWhileMEdgeCase2 :: Assertion
testTakeWhileMEdgeCase2 = do
    actual      <- H.takeWhileM (return . id) [False, False, False]
    let expected = takeWhile    (         id) [False, False, False]
    actual @?= expected

testTakeWhileMEdgeCase3 :: Assertion
testTakeWhileMEdgeCase3 = do
    let emptyList :: [Int] = []
    actual      <- H.takeWhileM (const undefined) emptyList
    let expected = takeWhile    (const undefined) emptyList
    actual @?= expected

testAssertCurrDirIsRepo :: Assertion
testAssertCurrDirIsRepo = do
    runEitherT $ H.init quietPrinter

    -- throws exception if fails
    result <- H.assertCurrDirIsRepo
    result @?= ()

filesystemTests :: TestTree
filesystemTests = testGroup "unit tests (Horse.Filesystem)"
    [ testCase
        "Testing `repoRoot` not in a repo"
        (runTest testNoRepoRepoRoot)
    , testCase
        "Testing `repoRoot` with no ancestors"
        (runTest testRepoRootNoAncestors)
    , testCase
        "Testing `destructivelyCreateDirectory`"
        (runTest testDestructivelyCreateDirectory)
    , testCase
        "Testing `getDirectoryContentsRecursiveSafe`"
        (runTest testGetDirectoryContentsRecursiveSafe)
    , testCase
        "Testing `dropPrefix`"
        testDropPrefix
    , testCase
        "Testing `dropUntil`"
        testDropUntil
    , testCase
        "Testing `takeWhileM`"
        testTakeWhileM
    , testCase
        "Testing `takeWhileM (edge case 1)`"
        testTakeWhileMEdgeCase1
    , testCase
        "Testing `takeWhileM (edge case 2)`"
        testTakeWhileMEdgeCase2
    , testCase
        "Testing `takeWhileM (edge case 3)`"
        testTakeWhileMEdgeCase3
    , testCase
        "Testing `assertCurrDirIsRepo`"
        (runTest testAssertCurrDirIsRepo)
    ]

testFromEitherMaybeDefault :: Assertion
testFromEitherMaybeDefault = do
    let a :: [Int] = H.fromEitherMaybeDefault (Left  2) Nothing
    let b :: Int   = H.fromEitherMaybeDefault (Right 3) Nothing
    let c :: Int   = H.fromEitherMaybeDefault (Left  2) (Just 4)
    let d :: Int   = H.fromEitherMaybeDefault (Right 2) (Just 4)

    a @?= []
    b @?= 3
    c @?= 4
    d @?= 4

utilsTests :: TestTree
utilsTests = testGroup "unit tests (Horse.Utils)"
    [ testCase
        "Testing `fromEitherMaybeDefault`"
        (runTest testFromEitherMaybeDefault)
    ]

testLoadCommitErrorCase :: Assertion
testLoadCommitErrorCase = do
    runEitherT $ H.init quietPrinter

    eitherCommit <- runEitherT $ H.loadCommit "xyz"
    eitherCommit @?= Left "Could not fetch commit for key \"xyz\"."

ioTests :: TestTree
ioTests = testGroup "unit tests (Horse.IO)"
    [ testCase
        "Testing `loadCommit` error case"
        (runTest testLoadCommitErrorCase)
    ]

testRelativeSyntaxErrorCase :: Assertion
testRelativeSyntaxErrorCase = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    eitherLogResult <- runEitherT $ H.log (Just "HEAD^~1") Nothing quietPrinter
    eitherLogResult @?= Left "Fatal: cannot combine '^' and '~' syntax."

testLogTooFarBackSyntax :: Assertion
testLogTooFarBackSyntax = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    eitherLogResult <- runEitherT $ H.log (Just "HEAD~2") Nothing quietPrinter
    eitherLogResult @?= Left "Fatal: specified relative commit is too far back in history; no commits exist there."

    eitherLogResult2 <- runEitherT $ H.log (Just "HEAD^^") Nothing quietPrinter
    eitherLogResult2 @?= Left "Fatal: specified relative commit is too far back in history; no commits exist there."

testUndefinedAncestorSyntax :: Assertion
testUndefinedAncestorSyntax = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    eitherLogResult <- runEitherT $ H.log (Just "HEAD~x") Nothing quietPrinter
    eitherLogResult @?= Left "Fatal: unrecognized syntax: ~x"

testLog :: Assertion
testLog = do
    runEitherT $ H.init quietPrinter
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

        history <- reverse <$> H.log (Just "HEAD") Nothing quietPrinter
        liftIO $ commits @?= history
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testLogEdgeCase1 :: Assertion
testLogEdgeCase1 = do
    runEitherT $ H.init quietPrinter
    eitherSuccess <- runEitherT $ do
        history <- reverse <$> H.log Nothing Nothing quietPrinter

        liftIO $ [] @?= history

    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testLogEdgeCase2 :: Assertion
testLogEdgeCase2 = do
    runEitherT $ H.init quietPrinter
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
        history <- reverse <$> H.log (Just ref) Nothing quietPrinter
        liftIO $ (take (2+1) commits) @?= history
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testLogEdgeCase3 :: Assertion
testLogEdgeCase3 = do
    runEitherT $ H.init quietPrinter
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
        history <- reverse <$> H.log (Just ref) Nothing quietPrinter
        liftIO $ ([head commits]) @?= history

        history2 <- H.log Nothing (Just 2) quietPrinter
        liftIO $ (length history2) @?= 2
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testLogEdgeCase4 :: Assertion
testLogEdgeCase4 = do
    runEitherT $ H.init quietPrinter
    eitherSuccess <- runEitherT $ do
        liftIO $ createFileWithContents "a" "a"
        H.stage "a"
        noargCommit

        liftIO $ createFileWithContents "b" "b"
        H.stage "b"
        noargCommit

        liftIO $ createFileWithContents "c" "c"
        H.stage "c"
        noargCommit

        liftIO $ createFileWithContents "d" "d"
        H.stage "d"
        noargCommit

        history <- reverse <$> H.log Nothing (Just 0) quietPrinter
        liftIO $ [] @?= history
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testStage :: Assertion
testStage = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"

    runEitherT $ H.stage "a"

    eitherStagingArea <- runEitherT H.loadStagingArea

    (Right ["a"]) @?= (adds <$> eitherStagingArea)
    (Right [])    @?= (mods <$> eitherStagingArea)
    (Right [])    @?= (dels <$> eitherStagingArea)

testStageDirectory :: Assertion
testStageDirectory = do
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"

    runEitherT $ H.stage "."

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea ["a"] [] []) [])

testStageDirectoryEdgeCase5 :: Assertion
testStageDirectoryEdgeCase5 = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"

    runEitherT $ H.stage "./"

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea ["a"] [] []) [])

testStageDirectoryEdgeCase6 :: Assertion
testStageDirectoryEdgeCase6 = do
    runEitherT $ H.init quietPrinter

    D.createDirectory "dir"
    createFileWithContents "a" "a"

    runEitherT $ H.stage "dir/.."

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea ["a"] [] []) [])

testStageNonexistentFile :: Assertion
testStageNonexistentFile = do
    runEitherT $ H.init quietPrinter

    eitherStagingArea <- runEitherT $ H.stage "xyz"

    eitherStagingArea @?= Left ("Can't stage file or directory at path \"xyz\"; no file or directory exists at that path, and no file was deleted at that path.")

testStageNonexistentDirectory :: Assertion
testStageNonexistentDirectory = do
    runEitherT $ H.init quietPrinter

    eitherStagingArea <- runEitherT $ H.stage "xyz"

    assertBool "Shouldn't stage a deletion of a nonexistent directory." (isLeft eitherStagingArea)

testStagePathOutsideOfRepo :: Assertion
testStagePathOutsideOfRepo = do
    runEitherT $ H.init quietPrinter

    eitherStagingArea <- runEitherT $ H.stage "../a"
    eitherStagingArea @?= Left "Can't stage file or directory outside of the repository: ../a"

testUnstagePathOutsideOfRepo :: Assertion
testUnstagePathOutsideOfRepo = do
    runEitherT $ H.init quietPrinter

    eitherStagingArea <- runEitherT $ H.unstage "../a"
    eitherStagingArea @?= Left "Can't unstage file or directory outside of the repository: ../a"

testStatusCase1 :: Assertion
testStatusCase1 = do
    runEitherT $ H.init quietPrinter

    eitherStatus <- runEitherT $ H.status quietPrinter

    assertBool "`status` command should not fail" (isRight eitherStatus)
    stagingArea <$> eitherStatus @?= Right def

testStatusCase2 :: Assertion
testStatusCase2 = do
    runEitherT $ H.init quietPrinter

    eitherStatus <- runEitherT $ H.status quietPrinter

    assertBool "`status` command should not fail" (isRight eitherStatus)
    unstagedFiles <$> eitherStatus @?= Right def

testStatusCase3 :: Assertion
testStatusCase3 = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    eitherStatus <- runEitherT $ H.status quietPrinter

    assertBool "`status` command should not fail" (isRight eitherStatus)
    let status = fromRight undefined eitherStatus

    stagingArea status @?= def
    unstagedFiles status @?= ["a"]
    return ()

testStatusCase4 :: Assertion
testStatusCase4 = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"

    runEitherT noargCommit

    eitherStatus <- runEitherT $ H.status quietPrinter
    assertBool "`status` command should not fail" (isRight eitherStatus)
    let status = fromRight undefined eitherStatus
    status @?= def

    createFileWithContents "b" "b"
    runEitherT $ H.stage "b"
    runEitherT noargCommit

    appendFile "a" "aaaa"

    eitherStatus <- runEitherT $ H.status quietPrinter
    assertBool "`status` command should not fail" (isRight eitherStatus)
    let status = fromRight undefined eitherStatus
    status @?= Status (StagingArea [] [] []) ["a"]

    return ()

testStatusCase5 :: Assertion
testStatusCase5 = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    appendFile "a" "bcd"
    runEitherT $ H.stage "a"
    eitherStatus <- runEitherT $ H.status quietPrinter
    assertBool "`status` command should not fail" (isRight eitherStatus)
    let status = fromRight undefined eitherStatus
    status @?= Status (StagingArea [] ["a"] []) []

    return ()

testInit :: Assertion
testInit = do
    runEitherT $ H.init quietPrinter
    rootDirectoryCreated <- D.doesDirectoryExist H.repositoryDataDir
    rootDirectoryCreated @?= True

testInitTwiceInSameDirectory :: Assertion
testInitTwiceInSameDirectory = do
    eitherInit1 <- runEitherT $ H.init quietPrinter
    eitherInit2 <- runEitherT $ H.init quietPrinter

    assertBool (fromLeft undefined eitherInit1) (isRight eitherInit1)
    assertBool "Fatal: command should fail" (isLeft eitherInit2)

testInitAgainInSubdir :: Assertion
testInitAgainInSubdir = do
    eitherInit1 <- runEitherT $ H.init quietPrinter
    D.createDirectory "x"
    D.setCurrentDirectory "x"
    eitherInit2 <- runEitherT $ H.init quietPrinter

    assertBool (fromLeft undefined eitherInit1) (isRight eitherInit1)
    eitherInit2 @?= Left "Fatal: directory is or is subdirectory of another horse-control repo"

    D.setCurrentDirectory ".."

testNoRepo :: (Eq a, Show a) => EitherT Error IO a -> Assertion
testNoRepo = (=<<) ((@?=) $ Left "Fatal: Not a horse repository (or any of the ancestor directories).") . runEitherT

testNoRepoStatus :: Assertion
testNoRepoStatus = testNoRepo $ H.status quietPrinter

testNoRepoStage :: Assertion
testNoRepoStage = testNoRepo $ H.stage def

testNoRepoCheckout :: Assertion
testNoRepoCheckout = testNoRepo $ H.checkout def

testNoRepoCommit :: Assertion
testNoRepoCommit = testNoRepo $ noargCommit

testNoRepoShow :: Assertion
testNoRepoShow = testNoRepo $ H.show def quietPrinter

testNoRepoLog :: Assertion
testNoRepoLog = testNoRepo $ H.log def def quietPrinter

testNoRepoSquash :: Assertion
testNoRepoSquash = testNoRepo $ H.squash def def

testNoRepoUnstage :: Assertion
testNoRepoUnstage = testNoRepo $ H.unstage def

testNoRepoUntrack :: Assertion
testNoRepoUntrack = testNoRepo $ H.untrack def quietPrinter

testNoRepoRetrack :: Assertion
testNoRepoRetrack = testNoRepo $ H.retrack def

testNoRepoListUntracked :: Assertion
testNoRepoListUntracked = testNoRepo $ H.listUntrackedPaths quietPrinter

testNoRepoDiff :: Assertion
testNoRepoDiff = testNoRepo $ H.diff quietPrinter

testNoRepoBranchList :: Assertion
testNoRepoBranchList = testNoRepo $ H.listBranches quietPrinter

testNoRepoBranchDelete :: Assertion
testNoRepoBranchDelete = testNoRepo $ H.deleteBranch def quietPrinter

testNoRepoBranchCreate :: Assertion
testNoRepoBranchCreate = testNoRepo $ H.createBranch def Nothing quietPrinter

--testNoRepoBranchSet :: Assertion
--testNoRepoBranchSet = testNoRepo $ H.setBranch def def quietPrinter

testCheckoutChangesHEAD :: Assertion
testCheckoutChangesHEAD = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherFirstCommit <- runEitherT noargCommit

    createFileWithContents "b" "2"
    runEitherT $ H.stage "b"
    eitherSecondCommit <- runEitherT noargCommit

    history1 <- runEitherT $ H.log Nothing Nothing quietPrinter
    length <$> history1 @?= Right 2

    let firstHash = hash $ fromRight undefined eitherFirstCommit
    runEitherT $ H.checkout (H.hashToString firstHash)
    history2 <- runEitherT $ H.log Nothing Nothing quietPrinter
    length <$> history2 @?= Right 1

testCheckout :: Assertion
testCheckout = do
    runEitherT $ H.init quietPrinter

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
quietCheckout ref = fromRight undefined <$> (runEitherT $ H.checkout ref)

testStatusFromSubdir :: Assertion
testStatusFromSubdir = do
    runEitherT $ H.init quietPrinter

    D.createDirectory "a"
    D.createDirectory "a/b"
    D.createDirectory "a/b/c"
    createFileWithContents "a/b/file" "x"
    D.setCurrentDirectory "a/b/c"

    eitherStatus <- runEitherT $ H.status quietPrinter
    assertBool "`status` command should not fail" (isRight eitherStatus)
    let status = fromRight undefined eitherStatus
    status @?= Status (StagingArea [] [] []) ["a/b/file"]

    D.setCurrentDirectory "../../.."

testStageFromSubdir :: Assertion
testStageFromSubdir = do
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.init quietPrinter

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
        quietCheckout ref = fromRight undefined <$> (runEitherT $ H.checkout ref)

testShowFromSubdir :: Assertion
testShowFromSubdir = do
    runEitherT $ H.init quietPrinter

    ----------------

    D.createDirectory "dir"
    D.setCurrentDirectory "dir"
    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit <- runEitherT noargCommit

    assertBool "`commit` should not fail." $ isRight eitherCommit
    let commit = fromRight undefined eitherCommit

    eitherShownCommit <- runEitherT $ H.show (Just $ H.hashToString $ hash commit) quietPrinter
    assertBool "`commit` should not fail." $ isRight eitherShownCommit
    let shownCommit = fromRight undefined eitherShownCommit

    shownCommit @?= commit

    D.setCurrentDirectory ".."

testShowNoArg :: Assertion
testShowNoArg = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit <- runEitherT noargCommit

    eitherShownCommit <- runEitherT $ H.show Nothing quietPrinter
    eitherShownCommit @?= eitherCommit

testLogFromSubdir :: Assertion
testLogFromSubdir = do
    runEitherT $ H.init quietPrinter
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

        history <- reverse <$> H.log Nothing Nothing quietPrinter
        liftIO $ commits @?= history
    D.setCurrentDirectory ".."
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testCommitAmend :: Assertion
testCommitAmend = do
    runEitherT $ H.init quietPrinter

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
    eitherCommit <- runEitherT $ H.commitAmend def Nothing quietPrinter

    ----------------

    eitherLog <- runEitherT $ H.log Nothing Nothing quietPrinter
    assertBool ("`log` should not fail: " ++ (fromLeft undefined eitherLog))  (isRight eitherLog)
    let log = fromRight undefined eitherLog

    length log @?= 1
    let squashedCommit = head log
    eitherCommit @?= Right squashedCommit

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

testCommitAmendNoPreviousCommits :: Assertion
testCommitAmendNoPreviousCommits = do
    runEitherT $ H.init quietPrinter

    ----------------

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit <- runEitherT $ H.commitAmend def Nothing quietPrinter

    eitherCommit @?= Left "Fatal: cannot amend when no commits have been made."

testCommitAmendFromSubdir :: Assertion
testCommitAmendFromSubdir = do
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.commitAmend def Nothing quietPrinter

    ----------------

    eitherLog <- runEitherT $ H.log Nothing Nothing quietPrinter
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
    runEitherT $ H.init quietPrinter

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

    runEitherT $ H.squash def (H.hashToString . hash $ firstCommit)

    eitherLog <- runEitherT $ H.log Nothing Nothing quietPrinter
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
    runEitherT $ H.init quietPrinter

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

    runEitherT $ H.squash def (H.hashToString . hash $ firstCommit)

    eitherLog <- runEitherT $ H.log Nothing Nothing quietPrinter
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
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "a"

    status <- getStatus
    status @?= Status (StagingArea ["a"] [] []) []

testStageSameFileTwiceWithChanges :: Assertion
testStageSameFileTwiceWithChanges = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"

    appendFile "a" "2"

    runEitherT $ H.stage "a"

    status <- getStatus
    status @?= Status (StagingArea ["a"] [] []) []

    return ()

testUnstage :: Assertion
testUnstage = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"

    firstStatus <- getStatus
    firstStatus @?= Status (StagingArea ["a"] [] []) []

    eitherStagingArea <- runEitherT $ H.unstage "a"
    eitherStagingArea @?= Right (StagingArea [] [] [])

    secondStatus <- getStatus
    secondStatus @?= Status (StagingArea [] [] []) ["a"]

testUnstageUnstagedFile :: Assertion
testUnstageUnstagedFile = do
    runEitherT $ H.init quietPrinter

    runEitherT $ H.stage "a"
    runEitherT $ H.unstage "a"

    status <- getStatus
    status @?= Status (StagingArea [] [] []) []

testUnstageNonexistentPath :: Assertion
testUnstageNonexistentPath = do
    runEitherT $ H.init quietPrinter

    runEitherT $ H.stage "a"
    runEitherT $ H.unstage "a"

    status <- getStatus
    status @?= Status (StagingArea [] [] []) []

testUnstageFromSubdir :: Assertion
testUnstageFromSubdir = do
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"

    eitherCommit <- runEitherT noargCommit

    eitherCommit @?= Left "Fatal: can't commit with an empty staging area."

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= (Right $ Status (StagingArea [] [] []) ["a"])

testStageFileWithNoChanges :: Assertion
testStageFileWithNoChanges = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= (Right $ Status (StagingArea [] [] []) [])

    eitherStagingArea <- runEitherT $ H.stage "a"
    eitherStagingArea @?= (Right $ StagingArea [] [] [])

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= (Right $ Status (StagingArea [] [] []) [])

testCheckoutTruncatedHash :: Assertion
testCheckoutTruncatedHash = do
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let commitHash = ByteString.take 8 . hash $ fromRight undefined eitherCommit1

    eitherCheckoutResult <- runEitherT $ H.checkout ""

    eitherCheckoutResult @?= Left "Fatal: can't untruncate the empty hash."

testCheckoutBadTruncatedHash2 :: Assertion
testCheckoutBadTruncatedHash2 = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    runEitherT noargCommit

    -- unlikely that this will be the actual hash
    eitherCheckoutResult <- runEitherT $ H.checkout "aaaaaaaa"

    eitherCheckoutResult @?= Left "Fatal: ref \"aaaaaaaa\" does not match any branch names or stored hashes"

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
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    runEitherT $ H.commit mockHasher1 Nothing quietPrinter

    createFileWithContents "b" "1"
    runEitherT $ H.stage "b"
    runEitherT $ H.commit mockHasher2 Nothing quietPrinter

    eitherCheckoutSuccess <- runEitherT $ H.checkout (replicate 9 'a')

    eitherCheckoutSuccess @?= Left "Fatal: multiple hashes or branch names match specified ref: [\"aaaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\",\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"]"

testCheckoutRelativeSyntaxCaret :: Assertion
testCheckoutRelativeSyntaxCaret = do
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.init quietPrinter

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
    runEitherT $ H.init quietPrinter

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

testUntrack :: Assertion
testUntrack = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.untrack "a" quietPrinter

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b"])

testUntrackGivenDirectory :: Assertion
testUntrackGivenDirectory = do
    runEitherT $ H.init quietPrinter

    D.createDirectory "x"
    D.createDirectory "x/y"
    createFileWithContents "x/a" "1"
    createFileWithContents "x/y/a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.untrack "x" quietPrinter

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b"])

testUntrackGivenDirectoryFromSubdir :: Assertion
testUntrackGivenDirectoryFromSubdir = do
    runEitherT $ H.init quietPrinter

    D.createDirectory "x"
    D.createDirectory "x/y"
    createFileWithContents "x/a" "1"
    createFileWithContents "x/y/a" "1"
    createFileWithContents "b" "1"

    D.setCurrentDirectory "x"
    runEitherT $ H.untrack "." quietPrinter
    D.setCurrentDirectory ".."

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b"])

testRetrack :: Assertion
testRetrack = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.untrack "a" quietPrinter
    runEitherT $ H.retrack "a"

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["a", "b"])

testRetrackGivenDirectory :: Assertion
testRetrackGivenDirectory = do
    runEitherT $ H.init quietPrinter

    D.createDirectory "x"
    D.createDirectory "x/y"
    createFileWithContents "x/a" "1"
    createFileWithContents "x/y/a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.untrack "x" quietPrinter
    runEitherT $ H.retrack "x"

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b", "x/a", "x/y/a"])

testRetrackGivenDirectoryFromSubdir :: Assertion
testRetrackGivenDirectoryFromSubdir = do
    runEitherT $ H.init quietPrinter

    D.createDirectory "x"
    D.createDirectory "x/y"
    createFileWithContents "x/a" "1"
    createFileWithContents "x/y/a" "1"
    createFileWithContents "b" "1"

    D.setCurrentDirectory "x"
    runEitherT $ H.untrack "." quietPrinter
    runEitherT $ H.retrack "."
    D.setCurrentDirectory ".."

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b", "x/a", "x/y/a"])

testStagingUntrackedFile :: Assertion
testStagingUntrackedFile = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.untrack "b" quietPrinter
    runEitherT $ H.stage "b"

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["a"])

testUntrackMultipleTimes :: Assertion
testUntrackMultipleTimes = do
    runEitherT $ H.init quietPrinter

    D.createDirectory "x"
    D.createDirectory "x/y"
    createFileWithContents "x/a" "1"
    createFileWithContents "x/y/a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.untrack "x/a" quietPrinter
    runEitherT $ H.untrack "x/y" quietPrinter

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b"])

testConfigFirstTime :: Assertion
testConfigFirstTime = do
    runEitherT $ H.init quietPrinter

    eitherPreviousConfig <- runEitherT $ H.config Nothing Nothing
    H.configPath >>= D.removeFile

    eitherConfig <- runEitherT $ H.config (Just "x") (Just "y")
    eitherConfig @?= Right (Config (UserInfo "x" "y"))

    assertBool "`config` should not fail" (isRight eitherPreviousConfig)
    let previousConfig = fromRight undefined eitherPreviousConfig
    let previousUserInfo = userInfo previousConfig
    void . runEitherT $ H.config (Just $ name previousUserInfo) (Just $ email previousUserInfo)

testConfigFirstTimeNoParams :: Assertion
testConfigFirstTimeNoParams = do
    runEitherT $ H.init quietPrinter

    eitherPreviousConfig <- runEitherT $ H.config Nothing Nothing
    H.configPath >>= D.removeFile

    eitherConfig <- runEitherT $ H.config Nothing Nothing
    eitherConfig @?= Right (Config (UserInfo def def))

    assertBool "`config` should not fail" (isRight eitherPreviousConfig)
    let previousConfig = fromRight undefined eitherPreviousConfig
    let previousUserInfo = userInfo previousConfig
    void . runEitherT $ H.config (Just $ name previousUserInfo) (Just $ email previousUserInfo)

testConfigNotFirstTime :: Assertion
testConfigNotFirstTime = do
    runEitherT $ H.init quietPrinter

    eitherPreviousConfig <- runEitherT $ H.config Nothing Nothing
    H.configPath >>= D.removeFile

    eitherConfig <- runEitherT $ H.config (Just "x") (Just "y")
    eitherConfig @?= Right (Config (UserInfo "x" "y"))

    eitherConfig2 <- runEitherT $ H.config (Just "x'") (Just "y'")
    eitherConfig2 @?= Right (Config (UserInfo "x'" "y'"))

    eitherConfig3 <- runEitherT $ H.config (Just "x''") Nothing
    eitherConfig3 @?= Right (Config (UserInfo "x''" "y'"))

    assertBool "`config` should not fail" (isRight eitherPreviousConfig)
    let previousConfig = fromRight undefined eitherPreviousConfig
    let previousUserInfo = userInfo previousConfig
    void . runEitherT $ H.config (Just $ name previousUserInfo) (Just $ email previousUserInfo)

testNoRepoConfig :: Assertion
testNoRepoConfig = do
    eitherPreviousConfig <- runEitherT $ H.config Nothing Nothing
    H.configPath >>= D.removeFile

    eitherConfig <- runEitherT $ H.config (Just "x") (Just "y")
    eitherConfig @?= Right (Config (UserInfo "x" "y"))

    assertBool "`config` should not fail" (isRight eitherPreviousConfig)
    let previousConfig = fromRight undefined eitherPreviousConfig
    let previousUserInfo = userInfo previousConfig
    void . runEitherT $ H.config (Just $ name previousUserInfo) (Just $ email previousUserInfo)

testUntrackPathOutsideOfRepo :: Assertion
testUntrackPathOutsideOfRepo = do
    runEitherT $ H.init quietPrinter

    eitherUnit <- runEitherT $ H.untrack "../a" quietPrinter

    eitherUnit @?= Left "Can't untrack file or directory outside of the repository: ../a"

testUngnorePathOutsideOfRepo :: Assertion
testUngnorePathOutsideOfRepo = do
    runEitherT $ H.init quietPrinter

    eitherUnit <- runEitherT $ H.retrack "../a"

    eitherUnit @?= Left "Can't retrack file or directory outside of the repository: ../a"

testRemovingUntrackedFile :: Assertion
testRemovingUntrackedFile = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    runEitherT $ H.untrack "a" quietPrinter

    D.removeFile "a"

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) [])

testStageCurrentDirectoryRemovedFile :: Assertion
testStageCurrentDirectoryRemovedFile = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    D.removeFile "a"

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["a"])

    runEitherT $ H.stage "."

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] ["a"]) [])

testRetrackingNeverUntrackedFile :: Assertion
testRetrackingNeverUntrackedFile = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    result <- runEitherT $ H.retrack "a"

    result @?= Right ()

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["a"])

testStagingHorseDir :: Assertion
testStagingHorseDir = do
    runEitherT $ H.init quietPrinter

    eitherStagingArea <- runEitherT $ H.stage ".horse"
    eitherStagingArea @?= Left "Fatal: cannot stage .horse; it is a directory required by horse-control."

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) [])

testUntrackingStagedFile :: Assertion
testUntrackingStagedFile = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT $ H.untrack "a" quietPrinter

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea ["a"] [] []) [])

testDiffFromSubdir :: Assertion
testDiffFromSubdir = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "."
    runEitherT noargCommit

    createFileWithContents "b" "b"

    D.createDirectory "x"
    D.setCurrentDirectory "x"
    eitherDiff <- runEitherT $ H.diff quietPrinter
    D.setCurrentDirectory ".."

    eitherDiff @?= (Right $
        FD.Diff
            { FD.filediffs =
                [ FD.Filediff
                    { FD.base = "b"
                    , FD.comp = "b"
                    , FD.change = FD.Add $ FD.ListDiff
                        { FD.dels = []
                        , FD.adds = [(0,"b")]
                        }
                    }
                ]
            })

testDiff :: Assertion
testDiff = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "."
    runEitherT noargCommit

    createFileWithContents "b" "b"

    eitherDiff <- runEitherT $ H.diff quietPrinter

    eitherDiff @?= (Right $
        FD.Diff
            { FD.filediffs =
                [ FD.Filediff
                    { FD.base = "b"
                    , FD.comp = "b"
                    , FD.change = FD.Add $ FD.ListDiff
                        { FD.dels = []
                        , FD.adds = [(0,"b")]
                        }
                    }
                ]
            })

testDiffNoCommitsHaveBeenMade :: Assertion
testDiffNoCommitsHaveBeenMade = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"

    eitherDiff <- runEitherT $ H.diff quietPrinter

    eitherDiff @?= Left "Fatal: can't diff with HEAD when no commits have been made."

testBranchListNewRepo :: Assertion
testBranchListNewRepo = do
    runEitherT $ H.init quietPrinter

    eitherBranches <- runEitherT $ H.listBranches quietPrinter

    eitherBranches @?= Right []

testInitialCommitCreatesNewBranch :: Assertion
testInitialCommitCreatesNewBranch = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "."
    eitherCommit <- runEitherT noargCommit

    assertBool (fromLeft undefined eitherCommit) (isRight eitherCommit)
    let commitHash = hash $ fromRight undefined eitherCommit

    eitherBranches <- runEitherT $ H.listBranches quietPrinter

    eitherBranches @?= Right [Branch "master" commitHash True]

testCommitAdvancesCurrentBranch :: Assertion
testCommitAdvancesCurrentBranch = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "."
    runEitherT noargCommit

    createFileWithContents "b" "b"
    runEitherT $ H.stage "."
    eitherCommit <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherCommit) (isRight eitherCommit)
    let commitHash = hash $ fromRight undefined eitherCommit

    eitherBranches <- runEitherT $ H.listBranches quietPrinter

    eitherBranches @?= Right [Branch "master" commitHash True]

testBranchCreateCreatesNewBranch :: Assertion
testBranchCreateCreatesNewBranch = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "."
    eitherCommit <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherCommit) (isRight eitherCommit)
    let commitHash = hash $ fromRight undefined eitherCommit

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "master" commitHash True]

    b <- runEitherT $ H.createBranch "newbranch" Nothing quietPrinter
    b @?= Right (Branch "newbranch" commitHash False)

    eitherBranches2 <- runEitherT $ H.listBranches quietPrinter

    eitherBranches2 @?= Right
        [ (Branch "newbranch" commitHash False)
        , (Branch "master"    commitHash True) ]

testBranchCreateCreatesNewBranchFromRef :: Assertion
testBranchCreateCreatesNewBranchFromRef = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "."
    eitherCommit1 <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherCommit1) (isRight eitherCommit1)
    let commitHash1 = hash $ fromRight undefined eitherCommit1

    createFileWithContents "b" "b"
    runEitherT $ H.stage "."
    eitherCommit2 <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherCommit2) (isRight eitherCommit2)
    let commitHash2 = hash $ fromRight undefined eitherCommit2

    runEitherT $ H.createBranch "newbranch" (Just "HEAD^") quietPrinter

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right
        [ (Branch "newbranch" commitHash1 False)
        , (Branch "master"    commitHash2 True) ]

testCannotDeleteCurrentBranch :: Assertion
testCannotDeleteCurrentBranch = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "."
    eitherCommit1 <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherCommit1) (isRight eitherCommit1)
    let commitHash1 = hash $ fromRight undefined eitherCommit1

    createFileWithContents "b" "b"
    runEitherT $ H.stage "."
    eitherCommit2 <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherCommit2) (isRight eitherCommit2)
    let commitHash2 = hash $ fromRight undefined eitherCommit2

    runEitherT $ H.createBranch "newbranch" (Just "HEAD^") quietPrinter

    success <- runEitherT $ H.deleteBranch "master" quietPrinter
    success @?= Left "Fatal: cannot delete current branch (master)"

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right
        [ (Branch "newbranch" commitHash1 False)
        , (Branch "master"    commitHash2 True) ]

testCanDeleteNoncurrentBranch :: Assertion
testCanDeleteNoncurrentBranch = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "."
    eitherCommit1 <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherCommit1) (isRight eitherCommit1)
    let commitHash1 = hash $ fromRight undefined eitherCommit1

    createFileWithContents "b" "b"
    runEitherT $ H.stage "."
    eitherCommit2 <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherCommit2) (isRight eitherCommit2)
    let commitHash2 = hash $ fromRight undefined eitherCommit2

    runEitherT $ H.createBranch "newbranch" (Just "HEAD^") quietPrinter

    success <- runEitherT $ H.deleteBranch "newbranch" quietPrinter
    success @?= Right ()

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "master" commitHash2 True]

testDeleteNonexistentBranch :: Assertion
testDeleteNonexistentBranch = do
    runEitherT $ H.init quietPrinter

    createFileWithContents "a" "a"
    runEitherT $ H.stage "."
    eitherCommit <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherCommit) (isRight eitherCommit)
    let commitHash = hash $ fromRight undefined eitherCommit


    success <- runEitherT $ H.deleteBranch "nonexistent" quietPrinter
    success @?= Left "Error: can't delete nonexistent branch \"nonexistent\""

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "master" commitHash True]

testLogGivenBranch :: Assertion
testLogGivenBranch = do
    eitherSuccess <- runEitherT $ do
        H.init quietPrinter

        liftIO $ createFileWithContents "a" "a"
        H.stage "a"
        commitA <- noargCommit
        bA <- H.createBranch "branch-a" Nothing quietPrinter

        liftIO $ createFileWithContents "b" "b"
        H.stage "b"
        commitB <- noargCommit
        bB <- H.createBranch "branch-b" Nothing quietPrinter

        liftIO $ createFileWithContents "c" "c"
        H.stage "c"
        commitC <- noargCommit
        bC <- H.createBranch "branch-c" Nothing quietPrinter

        liftIO $ createFileWithContents "d" "d"
        H.stage "d"
        commitD <- noargCommit
        bD <- H.createBranch "branch-d" Nothing quietPrinter

        let commits = [commitA, commitB, commitC, commitD]

        let ref = H.hashToString . hash $ head commits
        history <- reverse <$> H.log (Just "branch-c") Nothing quietPrinter
        liftIO $ ([commitA, commitB, commitC]) @?= history

    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testLogGivenBranchWithRelativeSyntax :: Assertion
testLogGivenBranchWithRelativeSyntax = do
    eitherSuccess <- runEitherT $ do
        H.init quietPrinter

        liftIO $ createFileWithContents "a" "a"
        H.stage "a"
        commitA <- noargCommit
        bA <- H.createBranch "branch-a" Nothing quietPrinter

        liftIO $ createFileWithContents "b" "b"
        H.stage "b"
        commitB <- noargCommit
        bB <- H.createBranch "branch-b" Nothing quietPrinter

        liftIO $ createFileWithContents "c" "c"
        H.stage "c"
        commitC <- noargCommit
        bC <- H.createBranch "branch-c" Nothing quietPrinter

        liftIO $ createFileWithContents "d" "d"
        H.stage "d"
        commitD <- noargCommit
        bD <- H.createBranch "branch-d" Nothing quietPrinter

        let commits = [commitA, commitB, commitC, commitD]

        let ref = H.hashToString . hash $ head commits
        history <- reverse <$> H.log (Just "branch-c^") Nothing quietPrinter
        liftIO $ ([commitA, commitB]) @?= history

    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testCheckoutGivenBranch :: Assertion
testCheckoutGivenBranch = do
    runEitherT $ H.init quietPrinter

    ----------------

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    runEitherT noargCommit
    runEitherT $ H.createBranch "branch-1" Nothing quietPrinter

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    runEitherT noargCommit
    runEitherT $ H.createBranch "branch-2" Nothing quietPrinter

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "3"
    D.removeFile "b"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    runEitherT noargCommit
    runEitherT $ H.createBranch "branch-3" Nothing quietPrinter

    ----------------

    -- try multiple combinations of gaps and orders and such
    test1 "branch-1"
    test2 "branch-2"
    test3 "branch-3"
    test2 "branch-2"
    test1 "branch-1"
    test3 "branch-3"
    test1 "branch-1"

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

testCheckoutGivenBranchWithRelativeSyntax :: Assertion
testCheckoutGivenBranchWithRelativeSyntax = do
    runEitherT $ H.init quietPrinter

    ----------------

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    runEitherT noargCommit
    runEitherT $ H.createBranch "branch-1" Nothing quietPrinter

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    runEitherT noargCommit
    runEitherT $ H.createBranch "branch-2" Nothing quietPrinter

    ----------------

    D.removeFile "a" >> createFileWithContents "a" "3"
    D.removeFile "b"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    runEitherT noargCommit
    runEitherT $ H.createBranch "branch-3" Nothing quietPrinter

    ----------------

    -- try multiple combinations of gaps and orders and such
    test1 "branch-2^"
    test2 "branch-3~1"
    test3 "branch-3~0"
    test2 "branch-2"
    test1 "branch-3^^"
    test3 "branch-3"
    test1 "branch-1"

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

testCheckoutChangesCurrentBranch :: Assertion
testCheckoutChangesCurrentBranch = do
    runEitherT $ H.init quietPrinter

    ----------------

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

    let stringHash1 = Just (H.hashToString hash1)
    let stringHash2 = Just (H.hashToString hash2)
    let stringHash3 = Just (H.hashToString hash3)

    runEitherT $ H.createBranch "branch-1" stringHash1 quietPrinter
    runEitherT $ H.createBranch "branch-2" stringHash2 quietPrinter
    runEitherT $ H.createBranch "branch-3" stringHash3 quietPrinter

    let branch1 = Right (Just (Branch "branch-1" hash1 True))
    let branch2 = Right (Just (Branch "branch-2" hash2 True))
    let branch3 = Right (Just (Branch "branch-3" hash3 True))

    -- try multiple combinations of gaps and orders and such
    test1 "branch-1" branch1
    test2 "branch-2" branch2
    test3 "branch-3" branch3
    test2 "branch-2" branch2
    test1 "branch-1" branch1
    test3 "branch-3" branch3
    test1 "branch-1" branch1

    return ()
    where
        test1 :: Hash -> (Either Error (Maybe Branch)) -> Assertion
        test1 hash expectedCurrentBranch = do
            quietCheckout . H.hashToString $ hash

            b <- runEitherT $ find isCurrentBranch <$> H.loadAllBranches
            b @?= expectedCurrentBranch

            aContents <- readFile "a"
            aContents @?= "1"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()

        test2 :: Hash -> (Either Error (Maybe Branch)) -> Assertion
        test2 hash expectedCurrentBranch = do
            quietCheckout . H.hashToString $ hash

            b <- runEitherT $ find isCurrentBranch <$> H.loadAllBranches
            b @?= expectedCurrentBranch

            aContents <- readFile "a"
            aContents @?= "2"

            aContents <- readFile "b"
            aContents @?= "2"
            return ()

        test3 :: Hash -> (Either Error (Maybe Branch)) -> Assertion
        test3 hash expectedCurrentBranch = do
            quietCheckout . H.hashToString $ hash

            b <- runEitherT $ find isCurrentBranch <$> H.loadAllBranches
            b @?= expectedCurrentBranch

            aContents <- readFile "a"
            aContents @?= "3"

            bExists <- D.doesFileExist "b"
            (not bExists) @? "`b` should not exist."
            return ()


-- test delete GCs

commandTests :: TestTree
commandTests = testGroup "unit tests (Horse.Commands)"
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
        "Testing command `untrack` run without a repo"
        (runTest testNoRepoUntrack)
    , testCase
        "Testing command `untrack --list` run without a repo"
        (runTest testNoRepoListUntracked)
    , testCase
        "Testing command `retrack` run without a repo"
        (runTest testNoRepoRetrack)
    , testCase
        "Testing command `config` run without a repo"
        (runTest testNoRepoConfig)
    , testCase
        "Testing command `diff` run without a repo"
        (runTest testNoRepoDiff)
    , testCase
        "Testing command `branch list` run without a repo"
        (runTest testNoRepoBranchList)
    , testCase
        "Testing command `branch delete` run without a repo"
        (runTest testNoRepoBranchDelete)
    , testCase
        "Testing command `branch create` run without a repo"
        (runTest testNoRepoBranchCreate)
    --, testCase
    --    "Testing command `branch set` run without a repo"
    --    (runTest testNoRepoBranchSet)
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
        "Testing `horse log` (edge case 4)"
        (runTest testLogEdgeCase4)
    , testCase
        "Testing `horse log` (edge case 5)"
        (runTest testLogTooFarBackSyntax)
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
        "Testing command `stage` (edge case 3)."
        (runTest testStageCurrentDirectoryRemovedFile)
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
        "Testing command `stage` when given a directory (edge case 7)."
        (runTest testStagingHorseDir)
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
        "Testing executing command `commit --amend` with no previous commits"
        (runTest testCommitAmendNoPreviousCommits)
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
        "Testing command `unstage` (edge case 3)"
        (runTest testUnstagePathOutsideOfRepo)
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
        "Testing command `untrack`"
        (runTest testUntrack)
    , testCase
        "Testing command `untrack` multiple times"
        (runTest testUntrackMultipleTimes)
    , testCase
        "Testing command `untrack` (given a directory)"
        (runTest testUntrackGivenDirectory)
    , testCase
        "Testing command `untrack` from a subdirectory"
        (runTest testUntrackGivenDirectoryFromSubdir)
    , testCase
        "Testing command `untrack` by untracking a file and then staging it"
        (runTest testStagingUntrackedFile)
    , testCase
        "Testing command `retrack`"
        (runTest testRetrack)
    , testCase
        "Testing command `retrack` (given a directory)"
        (runTest testRetrackGivenDirectory)
    , testCase
        "Testing command `retrack` from a subdirectory"
        (runTest testRetrackGivenDirectoryFromSubdir)
    , testCase
        "Testing command `config` (no previously existing config file)"
        (runTest testConfigFirstTime)
    , testCase
        "Testing command `config` (previously existing config file)"
        (runTest testConfigNotFirstTime)
    , testCase
        "Testing command `config` (first time, no params supplied)"
        (runTest testConfigFirstTimeNoParams)
    , testCase
        "Testing command `untrack` given a path outside of the repo"
        (runTest testUntrackPathOutsideOfRepo)
    , testCase
        "Testing command `retrack` given a path outside of the repo"
        (runTest testUngnorePathOutsideOfRepo)
    , testCase
        "Testing command `show` given no arguments"
        (runTest testShowNoArg)
    , testCase
        "Testing failure of combining ^ and ~ syntax"
        (runTest testRelativeSyntaxErrorCase)
    , testCase
        "Testing command `untrack` when removing a file."
        (runTest testRemovingUntrackedFile)
    , testCase
        "Testing command `retrack` when given a never-untracked file."
        (runTest testRetrackingNeverUntrackedFile)
    , testCase
        "Testing command `untrack` when given a staged file."
        (runTest testUntrackingStagedFile)
    , testCase
        "Testing command `diff`"
        (runTest testDiff)
    , testCase
        "Testing command `diff` run from a subdirectory"
        (runTest testDiffFromSubdir)
    , testCase
        "Testing command `diff` run without any commits made"
        (runTest testDiffNoCommitsHaveBeenMade)
    , testCase
        "Testing command `branch list` run after the first commit"
        (runTest testInitialCommitCreatesNewBranch)
    , testCase
        "Testing command `commit` advances current branch"
        (runTest testCommitAdvancesCurrentBranch)
    , testCase
        "Testing command `branch create` creates a new branch from HEAD"
        (runTest testBranchCreateCreatesNewBranch)
    , testCase
        "Testing command `branch create` creates a new branch from ref"
        (runTest testBranchCreateCreatesNewBranchFromRef)
    , testCase
        "Testing command `branch delete` when given the current branch"
        (runTest testCannotDeleteCurrentBranch)
    , testCase
        "Testing command `branch delete` when given a non-current branch"
        (runTest testCanDeleteNoncurrentBranch)
    , testCase
        "Testing command `branch delete` when given a nonexistent branch"
        (runTest testDeleteNonexistentBranch)
    , testCase
        "Testing command `log` when given a branch"
        (runTest testLogGivenBranch)
    , testCase
        "Testing command `log` when given a branch with relative syntax"
        (runTest testLogGivenBranchWithRelativeSyntax)
    , testCase
        "Testing command `checkout` when given a branch"
        (runTest testCheckoutGivenBranch)
    , testCase
        "Testing command `checkout` when given a branch with relative syntax"
        (runTest testCheckoutGivenBranchWithRelativeSyntax)
    , testCase
        "Testing undefined ancestor syntax"
        (runTest testUndefinedAncestorSyntax)
    , testCase
        "Testing command `checkout` changes current branch"
        (runTest testCheckoutChangesCurrentBranch)
    ]

tests :: TestTree
tests = testGroup "All tests" [commandTests, filesystemTests, ioTests, utilsTests]

main :: IO ()
main = defaultMain tests
