{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module encapsulating all printing to be done throughout command
--   execution.
module Horse.Printing
( printLog
, printCommitStats
, printCommit
, printStatus
, printDiff
, printBranches
) where

import Rainbow

import Data.Monoid
import Control.Applicative

import qualified Filediff.Types as FD
import qualified Filediff.Stats as FD
import qualified Filediff.Printing as FD

import qualified Data.ByteString.Char8 as ByteString

import Horse.Types
import Horse.Utils (hashToString)

printLog :: Maybe Hash -> [Commit] -> IO ()
printLog maybeHeadHash
    = mapM_ (\c ->
        printCommitInLog c
            (maybeBoolToBool $ ((==) $ hash c) <$> maybeHeadHash)
        )
    where
        maybeBoolToBool :: Maybe Bool -> Bool
        maybeBoolToBool (Just False) = False
        maybeBoolToBool (Just True) = True
        maybeBoolToBool Nothing = False

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
    putChunk   $ chunk ("    " :: ByteString)
    putChunkLn $ chunk (ByteString.pack message) & fore brightWhite
    putChunkLn $ chunk ("" :: ByteString)

    putChunkLn $ chunk ("diff --horse-control" :: ByteString) & bold
    FD.printDiff diff

printCommitStats :: Commit -> IO ()
printCommitStats commit = do
    putStrLn $ "[<branch> "
        ++ (Prelude.show . ByteString.take 7 $ hash commit)
        ++  "] " ++ (message commit)

    let numFiles = FD.numFilesAffected $ diffWithPrimaryParent commit
    let numAdds = FD.numAddedLines     $ diffWithPrimaryParent commit
    let numDels = FD.numDeletedLines   $ diffWithPrimaryParent commit
    let filesString = if numFiles == 1
        then " file"
        else " files"
    putStrLn $ ""
        ++ Prelude.show numFiles ++ filesString ++ " changed, "
        ++ Prelude.show numAdds ++ " insertions(+), "
        ++ Prelude.show numDels ++ " deletions(-)"

printStatus :: Status -> IO ()
printStatus (Status stagingArea unstagedFiles) = do
    putStrLn "Staged changes:"
    print stagingArea
    putStrLn ""

    putStrLn "Unstaged changes:"
    print unstagedFiles

printDiff :: FD.Diff -> IO ()
printDiff = FD.printDiff

printBranches :: [Branch] -> IO ()
printBranches branches = mapM_ (printBranch maxNameLength) branches
    where
        maxNameLength :: Int
        maxNameLength = maximum . map (length . branchName) $ branches

printBranch :: Int -> Branch -> IO ()
printBranch maxNameLength (Branch branchName branchHash isCurrentBranch) = do
    let prefix :: ByteString = if isCurrentBranch
        then "* "
        else "  "
    let padding :: ByteString = ByteString.pack $ replicate (maxNameLength - (length branchName)) ' '
    let name :: ByteString = (ByteString.pack branchName) <> padding
    let hash :: ByteString = " (" <> ByteString.take 7 branchHash <> ")"
    putChunk $ chunk prefix
    putChunk $ chunk name & fore green
    putChunkLn $ chunk hash
