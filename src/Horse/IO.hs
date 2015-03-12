{-# LANGUAGE PackageImports #-}

-- | Module for writing and reading from disk / the database
module Horse.IO
( -- * staging area
  loadStagingArea
, writeStagingArea

-- * HEAD
, loadHead
, writeHead

-- * commits
, loadCommit
, writeCommit

-- * config
, loadConfig
, writeConfig

-- * paths
, rootPath
, headPath
, stagingAreaPath
, diffsPath
, commitsPath
, getConfigPath

-- * lists
, directories
, databasePaths
, serializationPathsAndInitialContents

-- * utility functions
, createFileWithContents
, destructivelyCreateDirectory

-- * assorted
, loadHistory
, loadParent
) where

-- imports

import Prelude hiding (init, log)

import Control.Monad.Trans.Either

-- qualified imports

import qualified System.IO as IO
import qualified System.Directory as Dir

import qualified Data.Default as Default
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

-- imported functions

import Data.Maybe (isJust, fromJust)

import Control.Applicative ((<$>))
import Control.Monad ((>>=), return)
import "monad-extras" Control.Monad.Extra (iterateMaybeM)
import Control.Monad.IO.Class (liftIO, MonadIO(..))

-- horse-control imports

import Horse.Types
import Horse.Utils (maybeToEither, eitherToMaybe, stringToHash)

-- | Writes a serializable object to a file
writeToFile :: (Serialize.Serialize a) => FilePath -> a -> IO ()
writeToFile filepath
    = ByteString.writeFile filepath
    . Serialize.encode

-- | Loads a serializable object from a file
loadFromFile :: (Serialize.Serialize a) => FilePath -> EitherT Error IO a
loadFromFile filepath = EitherT $ Serialize.decode <$> ByteString.readFile filepath

-- * staging area

-- | Loads the staging area from disk
loadStagingArea :: EitherT Error IO StagingArea
loadStagingArea = loadFromFile stagingAreaPath

-- | Writes the staging area to disk
writeStagingArea :: StagingArea -> IO ()
writeStagingArea = writeToFile stagingAreaPath

-- * HEAD

-- | Loads the object representing HEAD from disk
loadHead :: EitherT Error IO Head
loadHead = loadFromFile headPath

-- | Writes the object representing HEAD to disk
writeHead :: Head -> IO ()
writeHead = writeToFile headPath

-- * commits

-- | Loads the commit with the given hash from the database
loadCommit :: Hash -> EitherT Error IO Commit
loadCommit key = do
    maybeCommit <- liftIO $ do
        db <- DB.open commitsPath Default.def
        maybeCommit <- DB.get db Default.def key
        DBInternal.unsafeClose db
        return maybeCommit
    commit <- hoistEither $ maybeToEither maybeCommit
    hoistEither $ Serialize.decode commit

-- | Writes the commit with the given hash to the database
writeCommit :: Commit -> Hash -> IO ()
writeCommit commit key = do
    db <- DB.open commitsPath Default.def
    DB.put db Default.def key (Serialize.encode commit)
    DBInternal.unsafeClose db

-- * config

-- | Loads the object representing user-specified configuration from disk
loadConfig :: EitherT Error IO Config
loadConfig = (liftIO getConfigPath) >>= loadFromFile

-- | Writes the object representing user-specified configuration to disk
writeConfig :: Config -> IO ()
writeConfig config = do
    configPath <- getConfigPath
    ByteString.writeFile configPath (Serialize.encode config)

-- * paths

-- | The root path for all (horse-config)-stored data
rootPath :: FilePath
rootPath = ".horse"

-- | The path to where the object representing HEAD is stored
headPath :: FilePath
headPath = rootPath ++ "/HEAD"

-- | The path to where the object representing the staging area is stored
stagingAreaPath :: FilePath
stagingAreaPath = rootPath ++ "/stagingArea"

-- | The path to where diffs are stored
diffsPath :: FilePath
diffsPath = rootPath ++ "/diffs"

-- | The path to where commits are stored
commitsPath :: FilePath
commitsPath = rootPath ++ "/commits"

-- | The path to where the object representing user-specified
-- | configuration information is stored. Returnvalue is wrapped in
-- | the `IO` monad because getting the user's home directory is
-- | a monadic operation.
getConfigPath :: IO FilePath
getConfigPath = ((++) "/.horseconfig") <$> Dir.getHomeDirectory

-- * lists

-- | Paths to directories created by the implementation
directories :: [FilePath]
directories = [rootPath, diffsPath, commitsPath]

-- | Paths to databases used in the implementation
databasePaths :: [FilePath]
databasePaths = [diffsPath, commitsPath]

-- | Paths to files used for storing objects, and the initial contents of
-- | those files upon initialization of an empty repository
serializationPathsAndInitialContents :: [(FilePath, ByteString.ByteString)]
serializationPathsAndInitialContents =
    [ (headPath, Serialize.encode $ (Default.def :: Head))
    , (stagingAreaPath, Serialize.encode $ (Default.def :: StagingArea)) ]

-- * utility functions

-- | Creates a file on disk with the specified content
createFileWithContents :: (FilePath, ByteString.ByteString) -> IO ()
createFileWithContents (path, contents) = do
    handle <- IO.openFile path IO.WriteMode
    ByteString.hPutStr handle contents
    IO.hClose handle

-- | Creates a directory on disk at the specified destination, 
-- | destroying one if it was already there
destructivelyCreateDirectory :: FilePath -> IO ()
destructivelyCreateDirectory dir = do
    dirAlreadyExists <- Dir.doesDirectoryExist dir
    if dirAlreadyExists
        then Dir.removeDirectoryRecursive dir
        else return ()
    Dir.createDirectory dir

-- * assorted

loadHistory :: Commit -> EitherT Error IO [Commit]
loadHistory commit
    = liftIO
    $ iterateMaybeM
        (fmap eitherToMaybe . runEitherT . loadParent)
        commit

loadParent :: Commit -> EitherT Error IO Commit
loadParent commit =
    if isJust $ parentHash commit
        then loadCommit
            . fromJust
            . parentHash
            $ commit
        else left $ "No parent for commit with hash " ++ (show . hash $ commit)

refToHash :: String -> EitherT Error IO Hash
refToHash unparsedRef = do
    base <- case baseRef of
        "HEAD" -> headHash <$> loadHead -- TODO: constant?
        someHash -> return $ stringToHash someHash
    final <- (hoistEither relatives) >>= applyRelatives base
    liftIO . putStrLn . show $ final
    right final
    where
        parentSyntax :: Char
        parentSyntax = '^'

        applyRelatives :: Hash -> [Relative] -> EitherT Error IO Hash
        applyRelatives h relatives = left ""

        baseRef :: String
        baseRef = takeWhile (not . isRelativeSyntax) unparsedRef

        relatives :: Either Error [Relative]
        relatives = mapM toRelative $ dropWhile (not . isRelativeSyntax) unparsedRef

        toRelative :: Char -> Either Error Relative
        toRelative ch = if ch == parentSyntax
            then Right Parent
            else Left "Undefined relative syntax"

        isRelativeSyntax :: Char -> Bool
        isRelativeSyntax ch = ch == parentSyntax
