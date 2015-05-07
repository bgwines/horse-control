{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

import System.Environment
import System.Exit

import qualified Horse.Types as Types
import qualified Horse.Commands as Commands

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

import Data.Either.Combinators (isLeft, fromLeft)

import Options.Applicative

import Data.Default (def)

-- | Schema for arguments to exposed commands
data Command
    = Init (Maybe Types.Verbosity)
    | Version
    | Config (Maybe String) (Maybe Types.EmailAddress)
    | Status (Maybe Types.Verbosity)
    | Stage String
    | Unstage String
    | Commit (Maybe String) Bool (Maybe Types.Verbosity)
    | Checkout String (Maybe Types.Verbosity)
    | Show (Maybe String)
    | Log (Maybe String) (Maybe Int) (Maybe Types.Verbosity)
    | Squash String
    | Untrack (Maybe String) Bool (Maybe Types.Verbosity)
    | Retrack String
    | Diff (Maybe Types.Verbosity)
    deriving (Show)

verbosityOption :: Parser (Maybe Types.Verbosity)
verbosityOption = (optional $ option auto
        ( long "verbosity"
        <> short 'v'
        <> metavar "VERBOSITY"
        <> help "How much logging to print during execution of the command" )
    )

parseInit :: Parser Command
parseInit = Init <$> verbosityOption

parseConfig :: Parser Command
parseConfig = Config
    <$> (optional $ strOption
            ( long "name"
            <> metavar "NAME"
            <> help "User's name, to be displayed in commit messages." )
        )
    <*> (optional $ strOption
            ( long "email"
            <> metavar "EMAIL"
            <> help "User's e-mail, to be displayed in commit messages." )
        )

parseStatus :: Parser Command
parseStatus = Status <$> verbosityOption

parseStage :: Parser Command
parseStage = Stage <$> (argument str $ metavar "FILE-OR-DIRECTORY")

parseUnstage :: Parser Command
parseUnstage = Unstage <$> (argument str $ metavar "FILE-OR-DIRECTORY")

parseCommit :: Parser Command
parseCommit = Commit
    <$> (optional $ strOption
            ( short 'm'
            <> metavar "COMMIT-MESSAGE" )
        )
    <*> switch (long "amend" <> help "Whether to amend the latest commit (HEAD)" )
    <*> verbosityOption

parseCheckout :: Parser Command
parseCheckout = Checkout
    <$> (argument str $ metavar "REF")
    <*> verbosityOption

parseShow :: Parser Command
parseShow = Show <$> (optional $ argument str
        ( metavar "REF"
        <> help "Ref to show" )
    )

parseLog :: Parser Command
parseLog = Log
    <$> (optional $ argument str
            ( metavar "REF"
            <> help "Ref from which to print log." )
        )
    <*> (optional $ option auto
            ( long "length"
            <> short 'n'
            <> metavar "HISTORY-LENGTH"
            <> help "Number of commits to display in history." )
        )
    <*> verbosityOption

parseVersion :: Parser Command
parseVersion = pure Version

parseSquash :: Parser Command
parseSquash = Squash <$> (argument str $ metavar "REF")

parseUntrack :: Parser Command
parseUntrack = Untrack
    <$> (optional $ argument str $ metavar "PATH")
    <*> switch (long "list" <> help "List untracked paths" )
    <*> verbosityOption

parseRetrack :: Parser Command
parseRetrack = Retrack <$> (argument str $ metavar "PATH")

parseDiff :: Parser Command
parseDiff = Diff <$> verbosityOption

parseCommand :: Parser Command
parseCommand = subparser
    $  command "version"  (parseVersion  `withInfo` versionHelpMessage)
    <> command "init"     (parseInit     `withInfo` initHelpMessage)
    <> command "config"   (parseConfig   `withInfo` configHelpMessage)
    <> command "status"   (parseStatus   `withInfo` statusHelpMessage)
    <> command "stage"    (parseStage    `withInfo` stageHelpMessage)
    <> command "unstage"  (parseUnstage  `withInfo` unstageHelpMessage)
    <> command "commit"   (parseCommit   `withInfo` commitHelpMessage)
    <> command "checkout" (parseCheckout `withInfo` checkoutHelpMessage)
    <> command "show"     (parseShow     `withInfo` showHelpMessage)
    <> command "log"      (parseLog      `withInfo` logHelpMessage)
    <> command "squash"   (parseSquash   `withInfo` squashHelpMessage)
    <> command "untrack"  (parseUntrack  `withInfo` untrackHelpMessage)
    <> command "retrack"  (parseRetrack  `withInfo` retrackHelpMessage)
    <> command "diff"     (parseDiff     `withInfo` diffHelpMessage)
        where
        initHelpMessage :: String
        initHelpMessage = "Initialize an empty repository"

        configHelpMessage :: String
        configHelpMessage = "Configure horse-control with custom settings"

        statusHelpMessage :: String
        statusHelpMessage = "Get information about the relationship between the current working directory and the current staging area"

        stageHelpMessage :: String
        stageHelpMessage = "Stage additions, modifications, or deletions to files."

        unstageHelpMessage :: String
        unstageHelpMessage = "Stage additions, modifications, or deletions to files."

        commitHelpMessage :: String
        commitHelpMessage = "Write the staging area as a commit"

        checkoutHelpMessage :: String
        checkoutHelpMessage = "Set HEAD to a ref and adjust the working directory accordingly"

        showHelpMessage :: String
        showHelpMessage = "Print the specified ref"

        logHelpMessage :: String
        logHelpMessage = "Print the commit history"

        versionHelpMessage :: String
        versionHelpMessage = "Show version"

        squashHelpMessage :: String
        squashHelpMessage = "Squash commits up to specified ref (non-inclusive)"

        untrackHelpMessage :: String
        untrackHelpMessage = "Untrack a file or directory (that is, be no longer able to stage modifications or deletions of the specified file)."

        retrackHelpMessage :: String
        retrackHelpMessage = "Retrack a file or directory (resume being able to stage modifications or deletions of the specified file)."

        diffHelpMessage :: String
        diffHelpMessage = "Print the difference between the working directory and HEAD."

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

run :: Command -> IO ()
run cmd = do
    eitherSuccess <- runEitherT $ case cmd of
        Version                  -> liftIO . putStrLn $ "0.1.0.0"
        Init v                   -> Commands.init v
        Checkout ref v           -> Commands.checkout ref v
        Config name email        -> void $ Commands.config name email
        Show ref                 -> void $ Commands.show ref Nothing
        Stage path               -> void $ Commands.stage path
        Unstage path             -> void $ Commands.unstage path
        Log ref n v              -> void $ Commands.log ref n v
        Status v                 -> void $ Commands.status v
        Commit msg False v       -> void $ Commands.commit def msg v
        Commit msg True v        -> void $ Commands.commitAmend def msg v
        Squash ref               -> void $ Commands.squash def ref
        Untrack _ True v         -> void $ Commands.listUntrackedPaths v
        Untrack (Just p) False v -> void $ Commands.untrack p v
        Retrack p                -> void $ Commands.retrack p
        Diff v                   -> void $ Commands.diff v
    if isLeft eitherSuccess
        then putStrLn $ fromLeft def eitherSuccess
        else return ()

main :: IO ()
main = run =<< execParser
    (parseCommand `withInfo` "Version control, written in Haskell with all that monadic goodness!")
