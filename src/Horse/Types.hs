{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Data types used by the implementation.
module Horse.Types(
-- * Records and their accessor functions
  UserInfo(..)
, Commit(..)
, hasParent

, StagingArea(..)
, files
, mapStagingArea
, isEmpty

, Config(..)

, Status(..)

, CommitHasher(..)

, Printer(..)
, quietPrinter
, normalPrinter

, Branch(..)
, makeCurrent
, makeNotCurrent

, EIO(..)

-- * Aliases
, EmailAddress
, Hash
, Date
, Error
) where

-- imports

import Prelude hiding (print, putStr, putStrLn, init, log)
import qualified Prelude as P (print, putStr, putStrLn)
import qualified Rainbow as R

import Control.Monad
import Control.Monad.Trans.Either

import GHC.Generics

import Data.Maybe (isJust)

import qualified Data.Hex as Hex

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Filediff as FD
import qualified Filediff.Types as FD
import qualified Filediff.Printing as FD

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)

import qualified Crypto.Hash.SHA256 as SHA256

-- imported functions

import Data.Char (toLower)

import Data.Serialize (Serialize, put, get)
import qualified Data.Serialize as Serialize (encode)

import Data.Default (Default, def)
import Data.ByteString (ByteString, empty)

instance Serialize T.Text where
    put = put . TE.encodeUtf8
    get = fmap TE.decodeUtf8 get

instance Serialize (FD.ListDiff T.Text)
instance Serialize FD.FileChange
instance Serialize FD.Filediff
instance Serialize FD.Diff

-- Not using `null` is the hack described below to increase code coverage.
{-# ANN module "HLint: ignore Use null" #-}

-- | Shorthand.
type EIO a = EitherT Error IO a

-- | Data type for any kind of error.
type Error = String

-- | An e-mail address.
type EmailAddress = String

-- | The hash type used for hashing commits and diffs.
type Hash = ByteString

instance Default Hash where
    def :: Hash
    def = empty

-- `toGregorian . utctDay` on a date
-- | Date information: (year, month, day).
type Date = (Integer, Int, Int)

-- | A record representing information about the user.
data UserInfo = UserInfo {
    name :: String,
    email :: String
} deriving (Eq, Generic)

instance Serialize UserInfo

instance Show UserInfo where
    show :: UserInfo -> String
    show (UserInfo userName userEmail) =
        userName ++ " <" ++ userEmail ++ ">"

-- | A commit record.
data Commit = Commit {
    author :: UserInfo,
    date :: Date,
    hash :: Hash,
    parentHash :: Maybe Hash,
    diffWithPrimaryParent :: FD.Diff,
    message :: String
} deriving (Eq, Show, Generic)

instance Serialize Commit

hasParent :: Commit -> Bool
hasParent = isJust . parentHash

-- | User-specific configuration information, analogous to what's
--   in ~/.gitconfig with Git.
data Config = Config {
    userInfo :: UserInfo
} deriving (Eq, Show, Generic)

instance Serialize Config

-- | The staging area: files with changes to include in an upcoming
--   commit.
data StagingArea = StagingArea {
    adds :: [FilePath],
    mods :: [FilePath],
    dels :: [FilePath]
} deriving (Eq, Show, Generic)

-- | Maps the given function over the list of adds, the list of mods,
--   and the list of dels.
mapStagingArea :: ([FilePath] -> [FilePath]) -> StagingArea -> StagingArea
mapStagingArea f (StagingArea adds mods dels) =
    StagingArea (f adds) (f mods) (f dels)

instance Serialize StagingArea

-- | Whether the specified staging area does not store any files.
isEmpty :: StagingArea -> Bool
isEmpty (StagingArea [] [] []) = True
isEmpty _ = False 

-- | Gets all files staged (be they added, modified, or deleted files).
files :: StagingArea -> [FilePath]
files (StagingArea adds mods dels) = adds ++ mods ++ dels

instance Default StagingArea where
    def :: StagingArea
    def = StagingArea { adds = def, mods = def, dels = def }

-- | The current status of the repository; essentially, the difference
--   between the current state of the filesystem, and the state it had
--   in HEAD.
data Status = Status {
    stagingArea :: StagingArea,
    unstagedFiles :: [FilePath]
} deriving (Eq, Show)

instance Default Status where
    def :: Status
    def = Status def def

-- | Encapsulation of hashing algorithm. Decomposed for mocking purposes
--   for testing. Is an instance of 'Default'; use that unless testing.
data CommitHasher = CommitHasher {
    hashingAlg :: Commit -> Hash
}

instance Default CommitHasher where
    def :: CommitHasher
    def = CommitHasher
        $ BS.pack
        . map (BS.c2w . toLower . BS.w2c)
        . BS.unpack
        . BS.take 40
        . Hex.hex
        . SHA256.hash
        . Serialize.encode

data Printer = Printer {
    putStr :: String -> IO (),
    putStrLn :: String -> IO (),
    putChunk :: R.Chunk ByteString -> IO (),
    putChunkLn :: R.Chunk ByteString -> IO (),
    printDiff :: FD.Diff -> IO ()
}

instance Show Printer where
    show :: Printer -> String
    show = const "Printer: an ADT wrapper around a set of functions to print to the console."

quietPrinter :: Printer
quietPrinter = Printer putStr' putStrLn' putChunk' putChunkLn' printDiff'
    where
        -- hack to get better code coverage but not actually print
        -- anything during tests
        putStr' str   = when (length str > 0) $ return ()
        putStrLn' str = when (length str > 0) $ return ()
        putChunk' ch    = when ((length . Prelude.show $ ch) > 0) $ return ()
        putChunkLn' ch  = when ((length . Prelude.show $ ch) > 0) $ return ()
        printDiff' diff = when ((length . Prelude.show $ diff) > 0) $ return ()

normalPrinter :: Printer
normalPrinter = Printer P.putStr P.putStrLn R.putChunk R.putChunkLn FD.printDiff

-- | A branch. Like Git branches, horse-control branches are pointers
--   to commit hashes.
data Branch = Branch {
    branchName :: String,
    branchHash :: Hash,
    isCurrentBranch :: Bool
} deriving (Eq, Ord, Show, Generic)

makeNotCurrent :: Branch -> Branch
makeNotCurrent b@(Branch name hash _) = Branch name hash False

makeCurrent :: Branch -> Branch
makeCurrent b@(Branch name hash _) = Branch name hash True

instance Serialize Branch
