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

import Data.Default as Default

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
parseShow = Show <$> (optional $ strOption (metavar "REF"))

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

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

run :: Command -> IO ()
run cmd = do
    eitherSuccess <- case cmd of
        Version            -> fmap Right $ putStrLn "0.1.0.2"
        Config name email  -> fmap Right $ Commands.config name email
        Init v             -> runEitherT $ Commands.init v
        Checkout ref v     -> runEitherT $ Commands.checkout ref v
        Show ref           -> runEitherT $ void $ Commands.show ref Nothing
        Stage path         -> runEitherT $ void $ Commands.stage path
        Unstage path       -> runEitherT $ void $ Commands.unstage path
        Log ref n v        -> runEitherT $ void $ Commands.log ref n v
        Status v           -> runEitherT $ void $ Commands.status v
        Commit msg False v -> runEitherT $ void $ Commands.commit msg v
        Commit msg True v  -> runEitherT $ void $ Commands.commitAmend msg v
        Squash ref         -> runEitherT $ void $ Commands.squash ref
    if isLeft eitherSuccess
        then putStrLn $ fromLeft Default.def eitherSuccess
        else return ()

main :: IO ()
main = run =<< execParser
    (parseCommand `withInfo` "Version control, written in Haskell with all that monadic goodness!")
