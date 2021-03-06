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
import qualified Horse.Refs as H
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
    initRepo

    -- throws exception if fails
    result <- H.assertCurrDirIsRepo
    result @?= ()

filesystemTests :: TestTree
filesystemTests = testGroup "unit tests (Horse.Filesystem)"
    [ testCase
        "`repoRoot` not in a repo"
        (runTest testNoRepoRepoRoot)
    , testCase
        "`repoRoot` with no ancestors"
        (runTest testRepoRootNoAncestors)
    , testCase
        "`destructivelyCreateDirectory`"
        (runTest testDestructivelyCreateDirectory)
    , testCase
        "`getDirectoryContentsRecursiveSafe`"
        (runTest testGetDirectoryContentsRecursiveSafe)
    , testCase
        "`dropPrefix`"
        testDropPrefix
    , testCase
        "`dropUntil`"
        testDropUntil
    , testCase
        "`takeWhileM`"
        testTakeWhileM
    , testCase
        "`takeWhileM (edge case 1)`"
        testTakeWhileMEdgeCase1
    , testCase
        "`takeWhileM (edge case 2)`"
        testTakeWhileMEdgeCase2
    , testCase
        "`takeWhileM (edge case 3)`"
        testTakeWhileMEdgeCase3
    , testCase
        "`assertCurrDirIsRepo`"
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

testHush :: Assertion
testHush = do
    let l :: Either Int () = Left 3
    let r :: Either () Int = Right 3
    H.hush l @?= Nothing
    H.hush r @?= Just 3

utilsTests :: TestTree
utilsTests = testGroup "unit tests (Horse.Utils)"
    [ testCase
        "`fromEitherMaybeDefault`"
        testFromEitherMaybeDefault
    , testCase
        "`hush`"
        testHush
    ]

testLoadBranchFromRefErrorCase :: Assertion
testLoadBranchFromRefErrorCase = do
    initRepo

    b <- runEitherT $ H.loadBranchFromRef "nonexistent"
    b @?= Left "Could not load branch from ref: nonexistent"

refsTests :: TestTree
refsTests = testGroup "unit tests (Horse.Refs)"
    [ testCase
        "`loadBranchFromRef`"
        (runTest testLoadBranchFromRefErrorCase)
    ]

testLoadCommitErrorCase :: Assertion
testLoadCommitErrorCase = do
    initRepo

    eitherCommit <- runEitherT $ H.loadCommit "xyz"
    eitherCommit @?= Left "Could not fetch commit for key \"xyz\"."

ioTests :: TestTree
ioTests = testGroup "unit tests (Horse.IO)"
    [ testCase
        "`loadCommit` error case"
        (runTest testLoadCommitErrorCase)
    ]

initRepo :: IO (Either String ())
initRepo = runEitherT $ H.init quietPrinter

testRelativeSyntaxErrorCase :: Assertion
testRelativeSyntaxErrorCase = do
    initRepo

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    eitherLogResult <- runEitherT $ H.log (Just "HEAD^~1") Nothing quietPrinter
    eitherLogResult @?= Left "Fatal: cannot combine '^' and '~' syntax."

testLogTooFarBackSyntax :: Assertion
testLogTooFarBackSyntax = do
    initRepo

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    eitherLogResult <- runEitherT $ H.log (Just "HEAD~2") Nothing quietPrinter
    eitherLogResult @?= Left "Fatal: specified relative commit is too far back in history; no commits exist there."

    eitherLogResult2 <- runEitherT $ H.log (Just "HEAD^^") Nothing quietPrinter
    eitherLogResult2 @?= Left "Fatal: specified relative commit is too far back in history; no commits exist there."

testUndefinedAncestorSyntax :: Assertion
testUndefinedAncestorSyntax = do
    initRepo

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    eitherLogResult <- runEitherT $ H.log (Just "HEAD~x") Nothing quietPrinter
    eitherLogResult @?= Left "Fatal: unrecognized syntax: ~x"

testLog :: Assertion
testLog = do
    initRepo
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
    initRepo
    eitherSuccess <- runEitherT $ do
        history <- reverse <$> H.log Nothing Nothing quietPrinter

        liftIO $ [] @?= history

    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft undefined eitherSuccess)
    return ()

testLogEdgeCase2 :: Assertion
testLogEdgeCase2 = do
    initRepo
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
    initRepo
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
    initRepo
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
    initRepo

    createFileWithContents "a" "a"

    runEitherT $ H.stage "a"

    eitherStagingArea <- runEitherT H.loadStagingArea

    (Right ["a"]) @?= (adds <$> eitherStagingArea)
    (Right [])    @?= (mods <$> eitherStagingArea)
    (Right [])    @?= (dels <$> eitherStagingArea)

testStageDirectory :: Assertion
testStageDirectory = do
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

    createFileWithContents "a" "a"

    runEitherT $ H.stage "."

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea ["a"] [] []) [])

testStageDirectoryEdgeCase5 :: Assertion
testStageDirectoryEdgeCase5 = do
    initRepo

    createFileWithContents "a" "a"

    runEitherT $ H.stage "./"

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea ["a"] [] []) [])

testStageDirectoryEdgeCase6 :: Assertion
testStageDirectoryEdgeCase6 = do
    initRepo

    D.createDirectory "dir"
    createFileWithContents "a" "a"

    runEitherT $ H.stage "dir/.."

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea ["a"] [] []) [])

testStageNonexistentFile :: Assertion
testStageNonexistentFile = do
    initRepo

    eitherStagingArea <- runEitherT $ H.stage "xyz"

    eitherStagingArea @?= Left ("Can't stage file or directory at path \"xyz\"; no file or directory exists at that path, and no file was deleted at that path.")

testStageNonexistentDirectory :: Assertion
testStageNonexistentDirectory = do
    initRepo

    eitherStagingArea <- runEitherT $ H.stage "xyz"

    assertBool "Shouldn't stage a deletion of a nonexistent directory." (isLeft eitherStagingArea)

testStagePathOutsideOfRepo :: Assertion
testStagePathOutsideOfRepo = do
    initRepo

    eitherStagingArea <- runEitherT $ H.stage "../a"
    eitherStagingArea @?= Left "Can't stage file or directory outside of the repository: ../a"

testUnstagePathOutsideOfRepo :: Assertion
testUnstagePathOutsideOfRepo = do
    initRepo

    eitherStagingArea <- runEitherT $ H.unstage "../a"
    eitherStagingArea @?= Left "Can't unstage file or directory outside of the repository: ../a"

testStatusCase1 :: Assertion
testStatusCase1 = do
    initRepo

    eitherStatus <- runEitherT $ H.status quietPrinter

    assertBool "`status` command should not fail" (isRight eitherStatus)
    stagingArea <$> eitherStatus @?= Right def

testStatusCase2 :: Assertion
testStatusCase2 = do
    initRepo

    eitherStatus <- runEitherT $ H.status quietPrinter

    assertBool "`status` command should not fail" (isRight eitherStatus)
    unstagedFiles <$> eitherStatus @?= Right def

testStatusCase3 :: Assertion
testStatusCase3 = do
    initRepo

    createFileWithContents "a" "a"
    eitherStatus <- runEitherT $ H.status quietPrinter

    assertBool "`status` command should not fail" (isRight eitherStatus)
    let status = fromRight undefined eitherStatus

    stagingArea status @?= def
    unstagedFiles status @?= ["a"]
    return ()

testStatusCase4 :: Assertion
testStatusCase4 = do
    initRepo

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
    initRepo

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
    initRepo
    rootDirectoryCreated <- D.doesDirectoryExist H.repositoryDataDir
    rootDirectoryCreated @?= True

testInitTwiceInSameDirectory :: Assertion
testInitTwiceInSameDirectory = do
    eitherInit1 <- initRepo
    eitherInit2 <- initRepo

    assertBool (fromLeft undefined eitherInit1) (isRight eitherInit1)
    assertBool "Fatal: command should fail" (isLeft eitherInit2)

testInitAgainInSubdir :: Assertion
testInitAgainInSubdir = do
    eitherInit1 <- initRepo
    D.createDirectory "x"
    D.setCurrentDirectory "x"
    eitherInit2 <- initRepo

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

testNoRepoCherryPick :: Assertion
testNoRepoCherryPick = testNoRepo $ H.cherryPick def def quietPrinter

testNoRepoFastForward :: Assertion
testNoRepoFastForward = testNoRepo $ H.fastForward def

--testNoRepoBranchSet :: Assertion
--testNoRepoBranchSet = testNoRepo $ H.setBranch def def quietPrinter

testCheckoutChangesHEAD :: Assertion
testCheckoutChangesHEAD = do
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit <- runEitherT noargCommit

    eitherShownCommit <- runEitherT $ H.show Nothing quietPrinter
    eitherShownCommit @?= eitherCommit

testLogFromSubdir :: Assertion
testLogFromSubdir = do
    initRepo
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
    initRepo

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
    initRepo

    ----------------

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit <- runEitherT $ H.commitAmend def Nothing quietPrinter

    eitherCommit @?= Left "Fatal: cannot amend when no commits have been made."

testCommitAmendFromSubdir :: Assertion
testCommitAmendFromSubdir = do
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "a"

    status <- getStatus
    status @?= Status (StagingArea ["a"] [] []) []

testStageSameFileTwiceWithChanges :: Assertion
testStageSameFileTwiceWithChanges = do
    initRepo

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"

    appendFile "a" "2"

    runEitherT $ H.stage "a"

    status <- getStatus
    status @?= Status (StagingArea ["a"] [] []) []

    return ()

testUnstage :: Assertion
testUnstage = do
    initRepo

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
    initRepo

    runEitherT $ H.stage "a"
    runEitherT $ H.unstage "a"

    status <- getStatus
    status @?= Status (StagingArea [] [] []) []

testUnstageNonexistentPath :: Assertion
testUnstageNonexistentPath = do
    initRepo

    runEitherT $ H.stage "a"
    runEitherT $ H.unstage "a"

    status <- getStatus
    status @?= Status (StagingArea [] [] []) []

testUnstageFromSubdir :: Assertion
testUnstageFromSubdir = do
    initRepo

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
    initRepo

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
    initRepo

    createFileWithContents "a" "1"

    eitherCommit <- runEitherT noargCommit

    eitherCommit @?= Left "Fatal: can't commit with an empty staging area."

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= (Right $ Status (StagingArea [] [] []) ["a"])

testStageFileWithNoChanges :: Assertion
testStageFileWithNoChanges = do
    initRepo

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
    initRepo

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
    initRepo

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let commitHash = ByteString.take 8 . hash $ fromRight undefined eitherCommit1

    eitherCheckoutResult <- runEitherT $ H.checkout ""

    eitherCheckoutResult @?= Left "Fatal: can't untruncate the empty hash."

testCheckoutBadTruncatedHash2 :: Assertion
testCheckoutBadTruncatedHash2 = do
    initRepo

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    runEitherT noargCommit

    -- unlikely that this will be the actual hash
    eitherCheckoutResult <- runEitherT $ H.checkout "aaaaaaaa"

    eitherCheckoutResult @?= Left "Fatal: ref aaaaaaaa does not match any branch names or stored hashes"

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

    createFileWithContents "a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.untrack "a" quietPrinter

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["b"])

testUntrackGivenDirectory :: Assertion
testUntrackGivenDirectory = do
    initRepo

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
    initRepo

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
    initRepo

    createFileWithContents "a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.untrack "a" quietPrinter
    runEitherT $ H.retrack "a"

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["a", "b"])

testRetrackGivenDirectory :: Assertion
testRetrackGivenDirectory = do
    initRepo

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
    initRepo

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
    initRepo

    createFileWithContents "a" "1"
    createFileWithContents "b" "1"

    runEitherT $ H.untrack "b" quietPrinter
    runEitherT $ H.stage "b"

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["a"])

testUntrackMultipleTimes :: Assertion
testUntrackMultipleTimes = do
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

    eitherUnit <- runEitherT $ H.untrack "../a" quietPrinter

    eitherUnit @?= Left "Can't untrack file or directory outside of the repository: ../a"

testUngnorePathOutsideOfRepo :: Assertion
testUngnorePathOutsideOfRepo = do
    initRepo

    eitherUnit <- runEitherT $ H.retrack "../a"

    eitherUnit @?= Left "Can't retrack file or directory outside of the repository: ../a"

testRemovingUntrackedFile :: Assertion
testRemovingUntrackedFile = do
    initRepo

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT noargCommit

    runEitherT $ H.untrack "a" quietPrinter

    D.removeFile "a"

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) [])

testStageCurrentDirectoryRemovedFile :: Assertion
testStageCurrentDirectoryRemovedFile = do
    initRepo

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
    initRepo

    createFileWithContents "a" "a"
    result <- runEitherT $ H.retrack "a"

    result @?= Right ()

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) ["a"])

testStagingHorseDir :: Assertion
testStagingHorseDir = do
    initRepo

    eitherStagingArea <- runEitherT $ H.stage ".horse"
    eitherStagingArea @?= Left "Fatal: cannot stage .horse; it is a directory required by horse-control."

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea [] [] []) [])

testUntrackingStagedFile :: Assertion
testUntrackingStagedFile = do
    initRepo

    createFileWithContents "a" "a"
    runEitherT $ H.stage "a"
    runEitherT $ H.untrack "a" quietPrinter

    eitherStatus <- runEitherT $ H.status quietPrinter
    eitherStatus @?= Right (Status (StagingArea ["a"] [] []) [])

testDiffFromSubdir :: Assertion
testDiffFromSubdir = do
    initRepo

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
    initRepo

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
    initRepo

    createFileWithContents "a" "a"

    eitherDiff <- runEitherT $ H.diff quietPrinter

    eitherDiff @?= Left "Fatal: can't diff with HEAD when no commits have been made."

testBranchListNewRepo :: Assertion
testBranchListNewRepo = do
    initRepo

    eitherBranches <- runEitherT $ H.listBranches quietPrinter

    eitherBranches @?= Right []

testInitialCommitCreatesNewBranch :: Assertion
testInitialCommitCreatesNewBranch = do
    initRepo

    createFileWithContents "a" "a"
    runEitherT $ H.stage "."
    eitherCommit <- runEitherT noargCommit

    assertBool (fromLeft undefined eitherCommit) (isRight eitherCommit)
    let commitHash = hash $ fromRight undefined eitherCommit

    eitherBranches <- runEitherT $ H.listBranches quietPrinter

    eitherBranches @?= Right [Branch "master" commitHash True]

testCommitAdvancesCurrentBranch :: Assertion
testCommitAdvancesCurrentBranch = do
    initRepo

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

testCommitWhenDetached :: Assertion
testCommitWhenDetached = do
    initRepo

    createFileWithContents "a" "a"
    runEitherT $ H.stage "."
    eitherCommit1 <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherCommit1) (isRight eitherCommit1)
    let commit1 = fromRight undefined eitherCommit1

    runEitherT $ H.checkout (H.hashToString . hash $ commit1)

    createFileWithContents "b" "b"
    runEitherT $ H.stage "."
    eitherCommit2 <- runEitherT noargCommit
    assertBool (fromLeft undefined eitherCommit2) (isRight eitherCommit2)
    let commit2 = fromRight undefined eitherCommit2

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "master" (hash commit1) False]

    eitherHistory <- runEitherT $ H.log Nothing Nothing quietPrinter
    eitherHistory @?= Right [commit2, commit1]

testBranchCreateCreatesNewBranch :: Assertion
testBranchCreateCreatesNewBranch = do
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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
    initRepo

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

testCheckoutDoesNotChangeCurrentBranch :: Assertion
testCheckoutDoesNotChangeCurrentBranch = do
    initRepo

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight undefined eitherCommit1

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2

    currentBranch <- runEitherT $ find isCurrentBranch <$> H.loadAllBranches
    currentBranch @?= Right (Just (Branch "master" hash2 True))

    runEitherT $ H.checkout "HEAD^"

    newCurrentBranch <- runEitherT $ find isCurrentBranch <$> H.loadAllBranches
    newCurrentBranch @?= Right Nothing

testCannotCreateSameBranchTwice :: Assertion
testCannotCreateSameBranchTwice = do
    initRepo

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let commitHash = hash $ fromRight undefined eitherCommit1

    b <- runEitherT $ H.createBranch "newbranch" Nothing quietPrinter
    b @?= Right (Branch "newbranch" commitHash False)    

    b' <- runEitherT $ H.createBranch "newbranch" Nothing quietPrinter
    b' @?= Left "Fatal: branch already exists: newbranch"

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "newbranch" commitHash False, Branch "master" commitHash True]

testCannotCreateDefaultBranch :: Assertion
testCannotCreateDefaultBranch = do
    initRepo

    createFileWithContents "a" "1"

    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let commitHash = hash $ fromRight undefined eitherCommit1

    b <- runEitherT $ H.createBranch "master" Nothing quietPrinter
    b @?= Left "Fatal: branch already exists: master"

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "master" commitHash True]

testCannotCreateBranchWithNoCommits :: Assertion
testCannotCreateBranchWithNoCommits = do
    initRepo

    b <- runEitherT $ H.createBranch "newbranch" Nothing quietPrinter
    b @?= Left "Fatal: cannot create branch when no commits have been made."

testCherryPick :: Assertion
testCherryPick = do
    initRepo

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight (error "a") eitherCommit1

    b <- runEitherT $ H.createBranch "newbranch" Nothing quietPrinter

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2

    runEitherT $ H.checkout "newbranch"

    createFileWithContents "c" "1"
    runEitherT $ H.stage "c"
    eitherCommit3 <- runEitherT noargCommit
    let hash3 = hash $ fromRight (error "c") eitherCommit3

    eitherCommit <- runEitherT $ H.cherryPick "master" def quietPrinter
    assertBool "cherry-pick should succeed" (isRight eitherCommit)
    let commit = fromRight undefined eitherCommit

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "newbranch" (hash commit) True, Branch "master" hash2 False]

testCherryPickInvalidRef :: Assertion
testCherryPickInvalidRef = do
    initRepo

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight (error "a") eitherCommit1

    b <- runEitherT $ H.createBranch "newbranch" Nothing quietPrinter

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2

    runEitherT $ H.checkout "newbranch"

    createFileWithContents "c" "1"
    runEitherT $ H.stage "c"
    eitherCommit3 <- runEitherT noargCommit
    let hash3 = hash $ fromRight (error "c") eitherCommit3

    eitherCommit <- runEitherT $ H.cherryPick "invalidref" def quietPrinter
    eitherCommit @?= Left "Fatal: ref invalidref does not match any branch names or stored hashes"

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "newbranch" hash3 True, Branch "master" hash2 False]

testCherryPickFromSubdir :: Assertion
testCherryPickFromSubdir = do
    initRepo

    D.createDirectory "dir"
    D.setCurrentDirectory "dir"

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight (error "a") eitherCommit1

    b <- runEitherT $ H.createBranch "newbranch" Nothing quietPrinter

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2

    runEitherT $ H.checkout "newbranch"

    createFileWithContents "c" "1"
    runEitherT $ H.stage "c"
    eitherCommit3 <- runEitherT noargCommit
    let hash3 = hash $ fromRight (error "c") eitherCommit3

    eitherCommit <- runEitherT $ H.cherryPick "master" def quietPrinter
    assertBool "cherry-pick should succeed" (isRight eitherCommit)
    let commit = fromRight undefined eitherCommit

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "newbranch" (hash commit) True, Branch "master" hash2 False]

    D.setCurrentDirectory ".."

testCherryPickWhenDetached :: Assertion
testCherryPickWhenDetached = do
    initRepo

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight (error "a") eitherCommit1

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2

    b <- runEitherT $ H.createBranch "snd-commit-ptr" Nothing quietPrinter
    runEitherT $ H.checkout (H.hashToString hash1)

    eitherCommit <- runEitherT $ H.cherryPick "snd-commit-ptr" def quietPrinter
    assertBool "cherry-pick should succeed" (isRight eitherCommit)
    let commit = fromRight undefined eitherCommit

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "snd-commit-ptr" hash2 False, Branch "master" hash2 False]

testFastForwardOtherBranchDoesNotExist :: Assertion
testFastForwardOtherBranchDoesNotExist = do
    initRepo

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight (error "a") eitherCommit1

    result <- runEitherT $ H.fastForward "nonexistent-branch"
    result @?= Left "Fatal: ref nonexistent-branch does not match any branch names or stored hashes"

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "master" hash1 True]

testFastForwardNoCommitsHaveBeenMade :: Assertion
testFastForwardNoCommitsHaveBeenMade = do
    initRepo

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight (error "a") eitherCommit1

    eitherBranchesBeforeFF <- runEitherT $ H.listBranches quietPrinter

    result <- runEitherT $ H.fastForward "some-branch"
    result @?= Left "Fatal: ref some-branch does not match any branch names or stored hashes"

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= eitherBranchesBeforeFF

testFastForward :: Assertion
testFastForward = do
    initRepo

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight (error "a") eitherCommit1

    b <- runEitherT $ H.createBranch "ahead-branch" Nothing quietPrinter
    runEitherT $ H.checkout "ahead-branch"

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "ahead-branch" hash2 True, Branch "master" hash1 False]

    runEitherT $ H.checkout "master"
    result <- runEitherT $ H.fastForward "ahead-branch"
    assertBool "fast-forward should succeed" (isRight result)

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    sort <$> eitherBranches @?= sort <$> Right [Branch "ahead-branch" hash2 False, Branch "master" hash2 True]

testFastForwardFromSubdir :: Assertion
testFastForwardFromSubdir = do
    initRepo

    D.createDirectory "d"
    D.setCurrentDirectory "d"

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight (error "a") eitherCommit1

    b <- runEitherT $ H.createBranch "ahead-branch" Nothing quietPrinter
    runEitherT $ H.checkout "ahead-branch"

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    eitherBranches @?= Right [Branch "ahead-branch" hash2 True, Branch "master" hash1 False]

    runEitherT $ H.checkout "master"
    result <- runEitherT $ H.fastForward "ahead-branch"
    assertBool "fast-forward should succeed" (isRight result)

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    sort <$> eitherBranches @?= sort <$> Right [Branch "ahead-branch" hash2 False, Branch "master" hash2 True]

    D.setCurrentDirectory ".."

testFastForwardWithSelf :: Assertion
testFastForwardWithSelf = do
    initRepo

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight (error "a") eitherCommit1

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2

    result <- runEitherT $ H.fastForward "master"
    assertBool "fast-forward should succeed" (isRight result)

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    sort <$> eitherBranches @?= sort <$> Right [Branch "master" hash2 True]

testFastForwardNoBranchIsCurrent :: Assertion
testFastForwardNoBranchIsCurrent = do
    initRepo

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight (error "a") eitherCommit1

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2

    runEitherT $ H.checkout "HEAD^"

    result <- runEitherT $ H.fastForward "master"
    result @?= Left "Fatal: can't fast-forward when you don't have a branch checked out."

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    sort <$> eitherBranches @?= sort <$> Right [Branch "master" hash2 False]

testFastForwardWhenNotAncestor :: Assertion
testFastForwardWhenNotAncestor = do
    initRepo

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight (error "a") eitherCommit1

    runEitherT $ H.createBranch "base" Nothing quietPrinter
    runEitherT $ H.createBranch "left" Nothing quietPrinter
    runEitherT $ H.checkout "left"

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2

    runEitherT $ H.checkout "base"
    runEitherT $ H.createBranch "right" Nothing quietPrinter
    runEitherT $ H.checkout "right"

    D.removeFile "a" >> createFileWithContents "a" "3"
    createFileWithContents "b" "3"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit3 <- runEitherT noargCommit
    let hash3 = hash $ fromRight undefined eitherCommit3

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    sort <$> eitherBranches @?= sort <$> Right
        [ Branch "base" hash1 False
        , Branch "master" hash1 False
        , Branch "left" hash2 False
        , Branch "right" hash3 True ]

    result <- runEitherT $ H.fastForward "left"
    result @?= Left "Fatal: can only fast-forward when HEAD is an ancestor of left"

    eitherBranches2 <- runEitherT $ H.listBranches quietPrinter
    sort <$> eitherBranches2 @?= sort <$> Right
        [ Branch "base" hash1 False
        , Branch "master" hash1 False
        , Branch "left" hash2 False
        , Branch "right" hash3 True ]

testFastForwardWhenNotAncestor2 :: Assertion
testFastForwardWhenNotAncestor2 = do
    initRepo

    createFileWithContents "a" "1"
    runEitherT $ H.stage "a"
    eitherCommit1 <- runEitherT noargCommit
    let hash1 = hash $ fromRight (error "a") eitherCommit1

    runEitherT $ H.createBranch "base" Nothing quietPrinter

    D.removeFile "a" >> createFileWithContents "a" "2"
    createFileWithContents "b" "2"

    runEitherT $ H.stage "a"
    runEitherT $ H.stage "b"
    eitherCommit2 <- runEitherT noargCommit
    let hash2 = hash $ fromRight undefined eitherCommit2

    eitherBranches <- runEitherT $ H.listBranches quietPrinter
    sort <$> eitherBranches @?= sort <$> Right
        [ Branch "base" hash1 False
        , Branch "master" hash2 True ]

    result <- runEitherT $ H.fastForward "base"
    result @?= Left "Fatal: can only fast-forward when HEAD is an ancestor of base"

    eitherBranches2 <- runEitherT $ H.listBranches quietPrinter
    sort <$> eitherBranches2 @?= sort <$> Right
        [ Branch "base" hash1 False
        , Branch "master" hash2 True ]

fastForwardTests :: TestTree
fastForwardTests = testGroup "fast-forward tests"
    [ testCase
        "`fast-forward`"
        (runTest testFastForward)
    , testCase
        "`fast-forward` run without a repository"
        (runTest testNoRepoFastForward)
    , testCase
        "`fast-forward` from a subdirectory"
        (runTest testFastForwardFromSubdir)
    , testCase
        "`fast-forward` when no commits have been made"
        (runTest testFastForwardNoCommitsHaveBeenMade)
    , testCase
        "`fast-forward` with self"
        (runTest testFastForwardWithSelf)
    , testCase
        "`fast-forward` when the other branch dosen't exist"
        (runTest testFastForwardOtherBranchDoesNotExist)
    , testCase
        "`fast-forward` when not an ancestor of the other branch"
        (runTest testFastForwardWhenNotAncestor)
    , testCase
        "`fast-forward` when not an ancestor of the other branch (case 2)"
        (runTest testFastForwardWhenNotAncestor2)
    , testCase
        "`fast-forward` when no branch is current"
        (runTest testFastForwardNoBranchIsCurrent) ]

allCommandTests :: TestTree
allCommandTests = testGroup "unit tests (Horse.Commands)"
    [ testCase
        "`status` run without a repo"
        (runTest testNoRepoStatus)
    , testCase
        "`stage` run without a repo"
        (runTest testNoRepoStage)
    , testCase
        "`checkout` run without a repo"
        (runTest testNoRepoCheckout)
    , testCase
        "`commit` run without a repo"
        (runTest testNoRepoCommit)
    , testCase
        "`show` run without a repo"
        (runTest testNoRepoShow)
    , testCase
        "`log` run without a repo"
        (runTest testNoRepoLog)
    , testCase
        "`squash` run without a repo"
        (runTest testNoRepoSquash)
    , testCase
        "`untrack` run without a repo"
        (runTest testNoRepoUntrack)
    , testCase
        "`untrack --list` run without a repo"
        (runTest testNoRepoListUntracked)
    , testCase
        "`retrack` run without a repo"
        (runTest testNoRepoRetrack)
    , testCase
        "`config` run without a repo"
        (runTest testNoRepoConfig)
    , testCase
        "`diff` run without a repo"
        (runTest testNoRepoDiff)
    , testCase
        "`branch list` run without a repo"
        (runTest testNoRepoBranchList)
    , testCase
        "`branch delete` run without a repo"
        (runTest testNoRepoBranchDelete)
    , testCase
        "`branch create` run without a repo"
        (runTest testNoRepoBranchCreate)
    , testCase
        "`cherry-pick` run without a repo"
        (runTest testNoRepoCherryPick)
    , testCase
        "`horse init`"
        (runTest testInit)
    , testCase
        "`horse init` (edge case 1)"
        (runTest testInitTwiceInSameDirectory)
    , testCase
        "`horse init` (edge case 2)"
        (runTest testInitAgainInSubdir)
    , testCase
        "`horse log`"
        (runTest testLog)
    , testCase
        "`horse log` (edge case 1)"
        (runTest testLogEdgeCase1)
    , testCase
        "`horse log` (edge case 2)"
        (runTest testLogEdgeCase2)
    , testCase
        "`horse log` (edge case 3)"
        (runTest testLogEdgeCase3)
    , testCase
        "`horse log` (edge case 4)"
        (runTest testLogEdgeCase4)
    , testCase
        "`horse log` (edge case 5)"
        (runTest testLogTooFarBackSyntax)
    , testCase
        "`status` (case 1)"
        (runTest testStatusCase1)
    , testCase
        "`status` (case 2)"
        (runTest testStatusCase2)
    , testCase
        "`status` (case 3)"
        (runTest testStatusCase3)
    , testCase
        "`status` (case 4)"
        (runTest testStatusCase4)
    , testCase
        "`status` (case 5)"
        (runTest testStatusCase5)
    , testCase
        "`checkout`"
        (runTest testCheckout)
    , testCase
        "executing `status` from subdirectory"
        (runTest testStatusFromSubdir)
    , testCase
        "executing `stage` from subdirectory"
        (runTest testStageFromSubdir)
    , testCase
        "executing `checkout` from subdirectory"
        (runTest testCheckoutFromSubdir)
    , testCase
        "executing `commit` from subdirectory"
        (runTest testCommitFromSubdir)
    , testCase
        "executing `show` from subdirectory"
        (runTest testShowFromSubdir)
    , testCase
        "executing `log` from subdirectory"
        (runTest testLogFromSubdir)
    , testCase
        "executing `unstage` from a subdirectory"
        (runTest testUnstageFromSubdir)
    , testCase
        "executing `commit --amend`"
        (runTest testCommitAmend)
    , testCase
        "executing `commit --amend` from a subdirectory"
        (runTest testCommitAmendFromSubdir)
    , testCase
        "executing `commit --amend` with no commits"
        (runTest testCommitAmendNoPreviousCommits)
    , testCase
        "executing `squash`"
        (runTest testSquash)
    , testCase
        "executing `squash` from a subdirectory"
        (runTest testSquashFromSubdir)
    , testCase
        "committing with no staged files"
        (runTest testCommitNoStagedFiles)
    , testCase
        "`show` given no arguments"
        (runTest testShowNoArg)
    , testCase
        "failure of combining ^ and ~ syntax"
        (runTest testRelativeSyntaxErrorCase) ]

stageTests :: TestTree
stageTests = testGroup "stage tests"
    [ testCase
        "`stage`"
        (runTest testStage)
    , testCase
        "`stage` (edge case 1)"
        (runTest testStagePathOutsideOfRepo)
    , testCase
        "`stage` (edge case 1)"
        (runTest testStageNonexistentFile)
    , testCase
        "`stage` (edge case 2)"
        (runTest testStageNonexistentDirectory)
    , testCase
        "`stage` (edge case 3)."
        (runTest testStageCurrentDirectoryRemovedFile)
    , testCase
        "`stage` when given a directory"
        (runTest testStageDirectory)
    , testCase
        "`stage` when given a directory (edge case 1)"
        (runTest testStageDirectoryEdgeCase1)
    , testCase
        "`stage` when given a directory (edge case 2)"
        (runTest testStageDirectoryEdgeCase2)
    , testCase
        "`stage` when given a directory (edge case 3)"
        (runTest testStageDirectoryEdgeCase3)
    , testCase
        "`stage` when given a directory (edge case 4)"
        (runTest testStageDirectoryEdgeCase4)
    , testCase
        "`stage` when given a directory (edge case 5)"
        (runTest testStageDirectoryEdgeCase5)
    , testCase
        "`stage` when given a directory (edge case 6)"
        (runTest testStageDirectoryEdgeCase6)
    , testCase
        "`stage` when given a directory (edge case 7)."
        (runTest testStagingHorseDir)
    , testCase
        "staging same file twice with no changes in between"
        (runTest testStageSameFileTwiceNoChanges)
    , testCase
        "staging same file twice with changes in between"
        (runTest testStageSameFileTwiceWithChanges)
    , testCase
        "staging a file with no changes"
        (runTest testStageFileWithNoChanges) ]

unstageTests :: TestTree
unstageTests = testGroup "unstage tests"
    [ testCase
        "`unstage`"
        (runTest testUnstage)
    , testCase
        "`unstage` (edge case 1)"
        (runTest testUnstageNonexistentPath)
    , testCase
        "`unstage` (edge case 2)"
        (runTest testUnstageUnstagedFile)
    , testCase
        "`unstage` (edge case 3)"
        (runTest testUnstagePathOutsideOfRepo)
    , testCase
        "`unstage` when given a directory"
        (runTest testUnstageDirectory) ]

checkoutTests :: TestTree
checkoutTests = testGroup "checkout tests"
    [ testCase
        "`checkout` changes HEAD"
        (runTest testCheckoutChangesHEAD)
    , testCase
        "`checkout` given with truncated hash"
        (runTest testCheckoutTruncatedHash)
    , testCase
        "`checkout` given a bad truncated hash (case 1)"
        (runTest testCheckoutBadTruncatedHash1)
    , testCase
        "`checkout` given a bad truncated hash (case 2)"
        (runTest testCheckoutBadTruncatedHash2)
    , testCase
        "`checkout` given colliding truncated hashes"
        (runTest testCheckoutCollidingTruncatedHashes)
    , testCase
        "`checkout` given relative hash (case 1)"
        (runTest testCheckoutRelativeSyntaxCaret)
    , testCase
        "`checkout` given relative hash (case 2)"
        (runTest testCheckoutRelativeSyntaxTilde)
    , testCase
        "`checkout` given truncated relative hash"
        (runTest testCheckoutTruncatedRelativeSyntax)
    , testCase
        "`checkout` given relative hash (edge case 1)"
        (runTest testCheckoutRelativeSyntaxTildeZero) ]

configTests :: TestTree
configTests = testGroup "config tests"
    [ testCase
        "`config` (no previously existing config file)"
        (runTest testConfigFirstTime)
    , testCase
        "`config` (previously existing config file)"
        (runTest testConfigNotFirstTime)
    , testCase
        "`config` (first time, no params supplied)"
        (runTest testConfigFirstTimeNoParams) ]

trackingTests :: TestTree
trackingTests = testGroup "tracking/untracking tests"
    [ testCase
        "`untrack`"
        (runTest testUntrack)
    , testCase
        "`untrack` multiple times"
        (runTest testUntrackMultipleTimes)
    , testCase
        "`untrack` (given a directory)"
        (runTest testUntrackGivenDirectory)
    , testCase
        "`untrack` from a subdirectory"
        (runTest testUntrackGivenDirectoryFromSubdir)
    , testCase
        "`untrack` by untracking a file and then staging it"
        (runTest testStagingUntrackedFile)
    , testCase
        "`untrack` given a path outside of the repo"
        (runTest testUntrackPathOutsideOfRepo)
    , testCase
        "`untrack` when removing a file."
        (runTest testRemovingUntrackedFile)
    , testCase
        "`untrack` when given a staged file."
        (runTest testUntrackingStagedFile)
    , testCase
        "`retrack`"
        (runTest testRetrack)
    , testCase
        "`retrack` (given a directory)"
        (runTest testRetrackGivenDirectory)
    , testCase
        "`retrack` from a subdirectory"
        (runTest testRetrackGivenDirectoryFromSubdir)
    , testCase
        "`retrack` given a path outside of the repo"
        (runTest testUngnorePathOutsideOfRepo)
    , testCase
        "`retrack` when given a never-untracked file."
        (runTest testRetrackingNeverUntrackedFile) ]

diffTests :: TestTree
diffTests = testGroup "diff tests"
    [ testCase
        "`diff`"
        (runTest testDiff)
    , testCase
        "`diff` run from a subdirectory"
        (runTest testDiffFromSubdir)
    , testCase
        "`diff` run without any commits made"
        (runTest testDiffNoCommitsHaveBeenMade) ]

branchTests :: TestTree
branchTests = testGroup "branch tests"
    [ testCase
        "`branch list` run after the first commit"
        (runTest testInitialCommitCreatesNewBranch)
    , testCase
        "`commit` advances current branch"
        (runTest testCommitAdvancesCurrentBranch)
    , testCase
        "`commit` when detached"
        (runTest testCommitWhenDetached)
    , testCase
        "`branch create` creates a new branch from HEAD"
        (runTest testBranchCreateCreatesNewBranch)
    , testCase
        "`branch create` creates a new branch from ref"
        (runTest testBranchCreateCreatesNewBranchFromRef)
    , testCase
        "`branch delete` when given the current branch"
        (runTest testCannotDeleteCurrentBranch)
    , testCase
        "`branch delete` when given a non-current branch"
        (runTest testCanDeleteNoncurrentBranch)
    , testCase
        "`branch delete` when given a nonexistent branch"
        (runTest testDeleteNonexistentBranch)
    , testCase
        "`log` when given a branch"
        (runTest testLogGivenBranch)
    , testCase
        "`log` when given a branch with relative syntax"
        (runTest testLogGivenBranchWithRelativeSyntax)
    , testCase
        "`checkout` when given a branch"
        (runTest testCheckoutGivenBranch)
    , testCase
        "`checkout` when given a branch with relative syntax"
        (runTest testCheckoutGivenBranchWithRelativeSyntax)
    , testCase
        "undefined ancestor syntax"
        (runTest testUndefinedAncestorSyntax)
    , testCase
        "`checkout` changes current branch when given a branch"
        (runTest testCheckoutChangesCurrentBranch)
    , testCase
        "`checkout` doesn't change curr. branch with bad input"
        (runTest testCheckoutDoesNotChangeCurrentBranch)
    , testCase
        "`branch create` branch already exists."
        (runTest testCannotCreateSameBranchTwice)
    , testCase
        "`branch create` when given the same branch twice"
        (runTest testCannotCreateSameBranchTwice)
    , testCase
        "`branch create` when given the default branch"
        (runTest testCannotCreateDefaultBranch)
    , testCase
        "`branch create` without any commits"
        (runTest testCannotCreateBranchWithNoCommits) ]

cherryPickTests :: TestTree
cherryPickTests = testGroup "cherry-pick tests"
    [ testCase
        "`cherry-pick`"
        (runTest testCherryPick)
    , testCase
        "`cherry-pick` given an invalid ref"
        (runTest testCherryPickInvalidRef)
    , testCase
        "`cherry-pick` from a subdirectory"
        (runTest testCherryPickFromSubdir)
    , testCase
        "`cherry-pick` when not on a branch"
        (runTest testCherryPickWhenDetached) ]

commandTests :: TestTree
commandTests = testGroup "command unit tests (Horse.Commands)"
    [ allCommandTests
    , configTests
    , checkoutTests
    , unstageTests
    , stageTests
    , trackingTests
    , diffTests
    , branchTests
    , cherryPickTests
    , fastForwardTests ]

-- cp invalid commit

-- cp self

-- logging when detached

-- test delete GCs

-- all fast-forward tests but for merges

tests :: TestTree
tests = testGroup "All tests" [filesystemTests, ioTests, utilsTests, refsTests, commandTests]

main :: IO ()
main = defaultMain tests
