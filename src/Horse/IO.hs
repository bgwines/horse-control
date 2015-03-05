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
) where

-- imports

import Prelude hiding (show, init, log)

import GHC.Generics

import Control.Monad.Trans.Either

-- qualified imports

import qualified Data.Convertible as Convert

import qualified System.IO as IO
import qualified System.Directory as Dir

import qualified Data.Hex as Hex
import qualified Data.Default as Default
import qualified Data.Serialize as Serialize
import qualified Data.ByteString as ByteString

import qualified Crypto.Hash.SHA256 as SHA256

import qualified Database.LevelDB.Base as DB
import qualified Database.LevelDB.Internal as DBInternal

-- imported functions

import Data.Either.Unwrap (fromLeft, fromRight)

import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Text.Printf (printf)

import Control.Monad ((>>=), return)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)

-- horse-control imports

import Horse.Types

-- | Writes a serializable object to a file
writeToFile :: (Serialize.Serialize a) => FilePath -> a -> EitherT Error IO ()
writeToFile filepath
    = liftIO
    . ByteString.writeFile filepath
    . Serialize.encode

-- | Loads a serializable object from a file
loadFromFile :: (Serialize.Serialize a) => FilePath -> EitherT Error IO a
loadFromFile filepath = EitherT $ Serialize.decode <$> ByteString.readFile filepath

-- * staging area

-- | Loads the staging area from disk
loadStagingArea :: EitherT Error IO StagingArea
loadStagingArea = loadFromFile stagingAreaPath

-- | Writes the staging area to disk
writeStagingArea :: StagingArea -> EitherT Error IO ()
writeStagingArea = writeToFile stagingAreaPath

-- * HEAD

-- | Loads the object representing HEAD from disk
loadHead :: EitherT Error IO Head
loadHead = loadFromFile headPath

-- | Writes the object representing HEAD to disk
writeHead :: Head -> EitherT Error IO ()
writeHead = writeToFile headPath

-- * commits

maybeToEither :: Maybe b -> Either Error b
maybeToEither Nothing = Left "Maybe conversion was from Nothing"
maybeToEither (Just x) = Right x

-- | Loads the commit with the given hash from the database
loadCommit :: Hash -> EitherT Error IO Commit
loadCommit key = do
    db <- liftIO $ DB.open commitsPath Default.def
    maybeCommit <- liftIO $ DB.get db Default.def key
    liftIO $ DBInternal.unsafeClose db
    EitherT . return $ (maybeToEither maybeCommit) >>= Serialize.decode

-- | Writes the commit with the given hash to the database
writeCommit :: Commit -> Hash -> EitherT Error IO ()
writeCommit commit key = liftIO $ do
    db <- DB.open commitsPath Default.def
    DB.put db Default.def key (Serialize.encode commit)
    DBInternal.unsafeClose db

-- * config

-- | Loads the object representing user-specified configuration from disk
loadConfig :: EitherT Error IO Config
loadConfig = getConfigPath >>= loadFromFile

-- | Writes the object representing user-specified configuration to disk
writeConfig :: Config -> EitherT Error IO ()
writeConfig config = do
    configPath <- getConfigPath
    liftIO $ ByteString.writeFile configPath (Serialize.encode config)

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
getConfigPath :: EitherT Error IO FilePath
getConfigPath = ((++) "/.horseconfig") <$> liftIO Dir.getHomeDirectory

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
createFileWithContents :: (FilePath, ByteString.ByteString) -> EitherT Error IO ()
createFileWithContents (path, contents) = liftIO $ do
    handle <- IO.openFile path IO.WriteMode
    ByteString.hPutStr handle contents
    IO.hClose handle

-- | Creates a directory on disk at the specified destination, 
-- | destroying one if it was already there
destructivelyCreateDirectory :: FilePath -> EitherT Error IO ()
destructivelyCreateDirectory dir = liftIO $ do
    dirAlreadyExists <- Dir.doesDirectoryExist dir
    if dirAlreadyExists
        then Dir.removeDirectoryRecursive dir
        else return ()
    Dir.createDirectory dir

