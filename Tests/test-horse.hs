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

getTestDirectory :: IO FilePath
getTestDirectory = fmap (map formatChar . Prelude.show) getCurrentTime
    where
        formatChar :: Char -> Char
        formatChar ' ' = '-'
        formatChar '.' = '-'
        formatChar ':' = '-'
        formatChar ch = ch

runTest :: Assertion -> Assertion
runTest t = do
    testDirectory <- getTestDirectory
    Dir.createDirectory testDirectory
    Dir.setCurrentDirectory testDirectory
    --------------------------------------
    t
    --------------------------------------
    Dir.setCurrentDirectory ".."
    Dir.removeDirectoryRecursive testDirectory

testLog :: Assertion
testLog = do
    H.init
    eitherSuccess <- runEitherT $ do
        let messages = map Just ["A", "B", "C", "D"]
        commits <- mapM H.commit' messages

        history <- reverse <$> HIO.loadHistory (last commits)

        liftIO $ commits @?= history
        liftIO $ (length commits) @?= (length messages) -- WLOG
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft "" eitherSuccess)
    return ()

testStage :: Assertion
testStage = do
    H.init

    let addedFile = "a"
    handle <- IO.openFile addedFile IO.WriteMode
    IO.hPutStr handle "a"
    IO.hClose handle

    Dir.getCurrentDirectory >>= putStrLn
    runEitherT $ H.stage addedFile
    Dir.getCurrentDirectory >>= putStrLn

    eitherStagingArea <- runEitherT HIO.loadStagingArea
    putStrLn . show $ eitherStagingArea

    (Right [addedFile]) @?= (adds <$> eitherStagingArea)

    (Right [])          @?= (mods <$> eitherStagingArea)

    (Right [])          @?= (dels <$> eitherStagingArea)

testInit :: Assertion
testInit = do
    H.init
    rootDirectoryCreated <- Dir.doesDirectoryExist HIO.rootPath
    rootDirectoryCreated @?= True

testNoRepo :: Assertion
testNoRepo = do
    -- *no* H.init
    status   <- runEitherT $ H.status
    stage    <- runEitherT $ H.stage    Default.def
    checkout <- runEitherT $ H.checkout Default.def
    commit   <- runEitherT $ H.commit   Default.def
    hshow    <- runEitherT $ H.hshow    Default.def
    log      <- runEitherT $ H.log      Default.def Default.def

    isLeft status    @?= True
    isLeft stage     @?= True
    isLeft checkout  @?= True
    isLeft commit    @?= True
    isLeft hshow     @?= True
    isLeft log       @?= True

testDiffsInCommits :: Assertion
testDiffsInCommits = do
    H.init

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
        "Testing commands run without a horse-control repository"
        (runTest testNoRepo) 
    , testCase
        "Testing diffs being stored in commits"
        (runTest testDiffsInCommits)]

main :: IO ()
main = defaultMain tests
