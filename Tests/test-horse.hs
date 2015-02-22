module Main where

import System.Exit

import Test.QuickCheck

import Data.Time.Clock
import Data.Time.Calendar

import System.Directory

import Horse.Commands.Porcelain as Porcelain
import Horse.Filesys as Filesys

getTestDirectory :: IO FilePath
getTestDirectory = fmap (map formatChar . Prelude.show) getCurrentTime
    where
        formatChar :: Char -> Char
        formatChar ' ' = '-'
        formatChar '.' = '-'
        formatChar ':' = '-'
        formatChar ch = ch

testPlumbing :: IO ()
testPlumbing = return ()

testInit :: IO Bool
testInit = do
    Porcelain.init []
    success <- doesDirectoryExist Filesys.rootPath
    putStrLn $ "empty repository creation successful? " ++ (Prelude.show success)
    return success

testPorcelain :: IO ()
testPorcelain = do
    testInit
    return ()

main :: IO ()
main = do
    testDirectory <- getTestDirectory
    createDirectory testDirectory
    setCurrentDirectory testDirectory

    testPlumbing
    testPorcelain

    setCurrentDirectory ".."
    removeDirectoryRecursive testDirectory

    exitSuccess