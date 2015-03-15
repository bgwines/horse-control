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
import qualified Horse.Commands.Porcelain as Porcelain

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
    Porcelain.init
    eitherSuccess <- runEitherT $ do
        let messages = map Just ["A", "B", "C", "D"]
        commits <- mapM Porcelain.commit' messages

        history <- reverse <$> HIO.loadHistory (last commits)

        liftIO $ commits @?= history
        liftIO $ (length commits) @?= (length messages) -- WLOG
    when (isLeft eitherSuccess) $ do
        assertFailure (fromLeft "" eitherSuccess)
    return ()

testStage :: Assertion
testStage = do
    Porcelain.init

    let addedFile = "a"
    handle <- IO.openFile addedFile IO.WriteMode
    IO.hPutStr handle "a"
    IO.hClose handle

    runEitherT $ Porcelain.stage addedFile

    eitherStagingArea <- runEitherT HIO.loadStagingArea

    (Right [addedFile]) @?= (adds <$> eitherStagingArea)

    (Right [])          @?= (mods <$> eitherStagingArea)

    (Right [])          @?= (dels <$> eitherStagingArea)

testInit :: Assertion
testInit = do
    Porcelain.init
    rootDirectoryCreated <- Dir.doesDirectoryExist HIO.rootPath
    rootDirectoryCreated @?= True

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
        (runTest testLog) ]

main :: IO ()
main = defaultMain tests
