{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module encapsulating all printing to be done throughout command
--   execution.
module Horse.Printing
( printCommit
) where

import Rainbow

import Data.Monoid

import qualified Data.ByteString.Char8 as ByteString

import Horse.Types
import Horse.Utils (hashToString)

printCommit :: Commit -> Bool -> IO ()
printCommit (Commit author date hash _ diff message) isHead = do
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