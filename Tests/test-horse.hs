module Main where

-- imports

import Test.HUnit
import Test.QuickCheck

import Control.Monad.Trans.Either

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

import Text.Printf (printf)

import System.Exit (exitSuccess)


import Data.Either.Unwrap (fromLeft, fromRight)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Control.Monad ((>>=), return)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)

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

testInit :: Test
testInit = TestCase $ do
    runEitherT Porcelain.init
    rootDirectoryCreated <- Dir.doesDirectoryExist HIO.rootPath
    assertBool "Empty repository root directory (./.horse) was not created" rootDirectoryCreated

testAddAndRm :: Test
testAddAndRm = TestCase $ do
    runEitherT Porcelain.init

    let addedFile = "a"
    handle <- IO.openFile addedFile IO.WriteMode
    IO.hPutStr handle "a"
    IO.hClose handle

    runEitherT $ Porcelain.stage addedFile

    stagingArea <- runEitherT HIO.loadStagingArea

    assertEqual
        "Should only have staged addition of the added file; no more; no less. "
        (Right [addedFile])
        (adds <$> stagingArea)
    assertEqual
        "Should not have staged modifications of files. "
        (Right [])
        (mods <$> stagingArea)
    assertEqual
        "Should not have staged deletions of files. "
        (Right [])
        (dels <$> stagingArea)

    -- rm stuff
    -- how does Git handle adding and rming the same file?

testPorcelain :: Test
testPorcelain = TestList
    [ TestLabel "horse init" testInit
    , TestLabel "horse add, rm" testAddAndRm ]

main :: IO Counts
main = do
    testDirectory <- getTestDirectory
    Dir.createDirectory testDirectory
    Dir.setCurrentDirectory testDirectory

    runTestTT testPorcelain

    Dir.setCurrentDirectory ".."
    Dir.removeDirectoryRecursive testDirectory

    exitSuccess
