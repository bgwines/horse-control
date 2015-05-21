
-- | Utility functions for parsing refs and loading things from them.
module Horse.Refs
( refToHash
, isBranchRef
, loadBranchFromRef
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either

import Data.Char (isDigit)
import Data.List (find)

import qualified Data.ByteString as BS (take)

import Horse.Utils (hashToString, note)
import Horse.Types
import qualified Horse.IO as HIO
import qualified Horse.Filesystem as HF

-- | Syntax for specifying a parent: '^'.
parentSyntax :: Char
parentSyntax = '^'

-- | Syntax for specifying an ancestor: '~'.
ancestorSyntax :: Char
ancestorSyntax = '~'

-- | Given any string, attempt to convert it to a hash.
--   Succeeds if the string is in a hash format, even if
--   the hash is not a key in the database (no commits / diffs
--   have been hashed with that key yet). Fails if the format
--   is unexpected. Acceptable formats are listed in user-facing
--   documentation.
refToHash :: String -> EIO Hash
refToHash unparsedRef = do
    liftIO HF.assertCurrDirIsRepo

    base <- case baseRef of
        "HEAD" -> HIO.loadHeadHash
        someHash -> untruncateBaseRef someHash
    hoistEither (toAncestorDistance relatives) >>= applyRelatives base
    where
        -- throws exception if `read` fails
        toAncestorDistance :: String -> Either Error Int
        toAncestorDistance r
            | Prelude.null r = Right 0
            | all (parentSyntax ==) r = Right $ length r
            | parentSyntax `elem` r =
                Left $ "Fatal: cannot combine '" ++ [parentSyntax] ++ "' and '" ++ [ancestorSyntax] ++ "' syntax."
            | all isDigit (tail r) = Right (read (tail r))
            | otherwise = Left ("Fatal: unrecognized syntax: " ++ r)

        applyRelatives :: Hash -> Int -> EIO Hash
        applyRelatives h ancestorDistance = do
                history <- HIO.loadCommit h >>= HIO.loadHistory
                when (ancestorDistance >= length history) $
                    left "Fatal: specified relative commit is too far back in history; no commits exist there."
                right $ hash (history !! ancestorDistance)

        baseRef :: String
        baseRef = takeWhile (not . isRelativeSyntax) unparsedRef

        relatives :: String
        relatives = dropWhile (not . isRelativeSyntax) unparsedRef

        isRelativeSyntax :: Char -> Bool
        isRelativeSyntax ch
            = (ch == parentSyntax) || (ch == ancestorSyntax)

        untruncateBaseRef :: String -> EIO Hash
        untruncateBaseRef baseRef = do
            when (Prelude.null baseRef) $
                left "Fatal: can't untruncate the empty hash."

            allHashes <- HIO.loadAllHashes
            let matchingHashes = filter ((==) baseRef . hashToString . BS.take (length baseRef)) allHashes

            allBranches <- HIO.loadAllBranches
            let matchingBranches = filter ((==) baseRef . branchName) allBranches

            let matching = matchingHashes ++ map branchHash matchingBranches

            case length matching of
                0 -> left $ "Fatal: ref " ++ Prelude.show baseRef ++ " does not match any branch names or stored hashes"
                1 -> right $ head matching
                _ -> left $ "Fatal: multiple hashes or branch names match specified ref: " ++ Prelude.show matching

isBranchRef :: String -> EIO Bool
isBranchRef ref =
    HIO.loadAllBranches >>= right . any ((==) ref . branchName)

loadBranchFromRef :: String -> EIO Branch
loadBranchFromRef ref =
    HIO.loadAllBranches
    >>= hoistEither
        . note ("Could not load branch from ref: " ++ ref)
        . find ((==) ref . branchName)