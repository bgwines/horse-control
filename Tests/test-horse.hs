module Main where

import System.Exit

import Test.QuickCheck

testPlumbing :: IO ()
testPlumbing = return ()

testPorcelain :: IO ()
testPorcelain = quickCheckWith stdArgs { maxSuccess = 500 } f
    where
        f :: String -> Bool
        f _ = True

main :: IO ()
main = do
    testPlumbing
    testPorcelain
    exitSuccess