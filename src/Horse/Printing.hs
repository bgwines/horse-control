{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module encapsulating all printing to be done throughout command
--   execution.
module Horse.Printing
( printCommitInLog
, printCommit
) where

import Rainbow

import Data.Monoid

import qualified Filediff.Printing as FD

import qualified Data.ByteString.Char8 as ByteString

import Horse.Types
import Horse.Utils (hashToString)

-- | Prints a commit in `log` format. The 'Bool' represents whether the
--   specified commit is `HEAD` or not.
printCommitInLog :: Commit -> Bool -> IO ()
printCommitInLog (Commit author date hash _ diff message) isHead = do
    let s :: ByteString = "* " <> shortHash <> " - " <> (ByteString.pack . show $ date)
    putChunk $ chunk s & bold

    let headAnnotation :: ByteString = if isHead
        then " (HEAD)"
        else ""
    putChunkLn $ chunk headAnnotation

    putChunk $ chunk ("|        " :: ByteString) & fore cyan
    putChunkLn $ chunk (ByteString.pack message) & fore brightWhite

    putChunk $ chunk ("|"    :: ByteString) & fore cyan
    putChunk $ chunk ("  - " :: ByteString) & fore brightWhite & bold
    putChunkLn $ chunk (ByteString.pack . show $ author) & fore brightWhite & bold
    where
        shortHash :: ByteString
        shortHash = ByteString.take 7 hash

-- | Prints a commit in `log` format. The 'Bool' represents whether the
--   specified commit is `HEAD` or not.
printCommit :: Commit -> IO ()
printCommit (Commit author date hash _ diff message) = do
    let hashLine :: ByteString = "commit " <> hash
    putChunkLn $ chunk hashLine & fore yellow

    let authorLine :: ByteString = "Author: " <> (ByteString.pack . show $ author)
    putChunkLn $ chunk authorLine

    let dateLine :: ByteString = "Date: " <> (ByteString.pack . show $ date)
    putChunkLn $ chunk dateLine

    putChunkLn $ chunk ("" :: ByteString)
    putChunk $ chunk ("    " :: ByteString)
    putChunkLn $ chunk (ByteString.pack message) & fore brightWhite
    putChunkLn $ chunk ("" :: ByteString)

    putChunkLn $ chunk ("diff --horse-control" :: ByteString) & fore brightWhite
    FD.printDiff diff
