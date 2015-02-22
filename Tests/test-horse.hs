module Main where

import System.Exit

import Test.QuickCheck

import Data.Time.Clock
import Data.Time.Calendar

import System.Directory

import Horse.Commands.Porcelain as Porcelain
import Horse.Filesys as Filesys

import Test.HUnit

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
    Porcelain.init []
    rootDirectoryCreated <- doesDirectoryExist Filesys.rootPath
    assertBool "Empty repository root directory (./.horse) was not created" rootDirectoryCreated

testPlumbing :: Test
testPlumbing = TestList []

testPorcelain :: Test
testPorcelain = TestList
    [ TestLabel "horse init" testInit ]

main :: IO Counts
main = do
    testDirectory <- getTestDirectory
    createDirectory testDirectory
    setCurrentDirectory testDirectory

    runTestTT testPlumbing
    runTestTT testPorcelain

    setCurrentDirectory ".."
    removeDirectoryRecursive testDirectory

    exitSuccess
