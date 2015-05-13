{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module encapsulating all printing to be done throughout command
--   execution.
module Horse.Printing
( printLog
, printCommitStats
, printCommit
, printStatus
, Horse.Printing.printDiff
, printBranches
) where

import Prelude hiding (print, putStr, putStrLn)
import qualified Prelude (print)

import Rainbow hiding (putChunk, putChunkLn)

import Data.Monoid
import Control.Applicative

import qualified Filediff.Types as FD
import qualified Filediff.Stats as FD

import qualified Data.ByteString.Char8 as ByteString

import Horse.Types
import Horse.Utils (hashToString)

printLog :: Printer -> Hash -> [Commit] -> [Branch] -> IO ()
printLog printer headHash commits branches
    = mapM_ (\c ->
        printCommitInLog printer c
            (hash c == headHash)
            ( map branchName
            . filter ((==) (hash c) . branchHash)
            $ branches )
        )
        commits

-- | Prints a commit in `log` format. The 'Bool' represents whether the
--   specified commit is `HEAD` or not.
printCommitInLog :: Printer -> Commit -> Bool -> [String] -> IO ()
printCommitInLog
    (Printer putStr putStrLn putChunk putChunkLn _)
    (Commit author date hash _ diff message)
    isHead
    branchNames = do
    let s :: ByteString = "* " <> shortHash <> " - " <> (ByteString.pack . show $ date)
    putChunk $ chunk s & bold

    putChunkLn $ chunk (" " <> referencesAnnotation) & bold

    putChunk $ chunk ("|        " :: ByteString) & fore cyan
    putChunkLn $ chunk (ByteString.pack message) & fore brightWhite

    putChunk $ chunk ("|"    :: ByteString) & fore cyan
    putChunk $ chunk ("  - " :: ByteString) & fore brightWhite & bold
    putChunkLn $ chunk (ByteString.pack . show $ author) & fore brightWhite & bold
    where
        shortHash :: ByteString
        shortHash = ByteString.take 7 hash

        references :: [String]
        references = if isHead
            then "HEAD" : branchNames
            else branchNames

        referencesAnnotation :: ByteString
        referencesAnnotation = if null references
            then ""
            else ByteString.pack
                . (\a -> "(" ++ a ++ ")")
                . (\a -> take (length a - 2) a) -- for trailing ", "
                $ foldl (flip (++) . (++) ", ") "" references

-- | Prints a commit in `log` format. The 'Bool' represents whether the
--   specified commit is `HEAD` or not.
printCommit :: Printer -> Commit -> IO ()
printCommit
    (Printer putStr putStrLn putChunk putChunkLn printDiff)
    (Commit author date hash _ diff message)
    = do
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
    printDiff diff

printCommitStats :: Printer -> Commit -> IO ()
printCommitStats (Printer putStr putStrLn putChunk putChunkLn _) commit = do
    putStrLn $ "[<branch> "
        ++ (Prelude.show . ByteString.take 7 $ hash commit)
        ++  "] " ++ (message commit)

    let numFiles = FD.numFilesAffected $ diffWithPrimaryParent commit
    let numAdds  = FD.numAddedLines    $ diffWithPrimaryParent commit
    let numDels  = FD.numDeletedLines  $ diffWithPrimaryParent commit
    let filesString = if numFiles == 1
        then " file"
        else " files"
    putStrLn $ ""
        ++ Prelude.show numFiles ++ filesString ++ " changed, "
        ++ Prelude.show numAdds ++ " insertions(+), "
        ++ Prelude.show numDels ++ " deletions(-)"

printStatus :: Printer -> Status -> IO ()
printStatus
    (Printer putStr putStrLn putChunk putChunkLn _)
    (Status stagingArea unstagedFiles)
    = do
    putStrLn "Staged changes:"
    putStrLn . Prelude.show $ stagingArea
    putStrLn ""

    putStrLn "Unstaged changes:"
    putStrLn . Prelude.show $ unstagedFiles

printDiff :: Printer -> FD.Diff -> IO ()
printDiff (Printer _ _ _ _ printDiff) = printDiff

printBranches :: Printer -> [Branch] -> IO ()
printBranches printer branches = mapM_ (printBranch printer maxNameLength) branches
    where
        maxNameLength :: Int
        maxNameLength = maximum . map (length . branchName) $ branches

printBranch :: Printer -> Int -> Branch -> IO ()
printBranch
    (Printer putStr putStrLn putChunk putChunkLn _)
    maxNameLength
    (Branch branchName branchHash isCurrentBranch)
    = do
    let prefix :: ByteString = if isCurrentBranch
        then "* "
        else "  "
    let padding :: ByteString = ByteString.pack $ replicate (maxNameLength - (length branchName)) ' '
    let name :: ByteString = (ByteString.pack branchName) <> padding
    let hash :: ByteString = " (" <> ByteString.take 7 branchHash <> ")"
    putChunk $ chunk prefix
    putChunk $ chunk name & fore green
    putChunkLn $ chunk hash
