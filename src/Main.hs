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
    = Init                               Types.Printer
    | Version
    | Config (Maybe String) (Maybe Types.EmailAddress)
    | Status                             Types.Printer
    | Stage String
    | Unstage String
    | Commit (Maybe String) Bool         Types.Printer
    | Checkout String                    Types.Printer
    | Show (Maybe String)                Types.Printer
    | Log (Maybe String) (Maybe Int)     Types.Printer
    | Squash String
    | Untrack (Maybe String) Bool        Types.Printer
    | Retrack String
    | Diff                               Types.Printer
    | CreateBranch String (Maybe String) Types.Printer
    | DeleteBranch String                Types.Printer
    | ListBranches                       Types.Printer
    deriving (Show)

quietOption :: Parser Types.Printer
quietOption = toPrinter <$> (switch
        ( long "quiet"
        <> short 'q'
        <> help "Whether to supress logging to the console during the execution of the command." ) )
    where
        toPrinter :: Bool -> Types.Printer
        toPrinter False = Types.normalPrinter
        toPrinter True = Types.quietPrinter
parseInit :: Parser Command
parseInit = Init <$> quietOption

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
parseStatus = Status <$> quietOption

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
    <*> quietOption

parseCheckout :: Parser Command
parseCheckout = Checkout
    <$> (argument str $ metavar "REF")
    <*> quietOption

parseShow :: Parser Command
parseShow = Show
    <$> (optional $ argument str
            ( metavar "REF"
            <> help "Ref to show" )
        )
    <*> quietOption

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
    <*> quietOption

parseVersion :: Parser Command
parseVersion = pure Version

parseSquash :: Parser Command
parseSquash = Squash <$> (argument str $ metavar "REF")

parseUntrack :: Parser Command
parseUntrack = Untrack
    <$> (optional $ argument str $ metavar "PATH")
    <*> switch (long "list" <> help "List untracked paths" )
    <*> quietOption

parseCreateBranch :: Parser Command
parseCreateBranch = CreateBranch
    <$> (argument str $ metavar "BRANCH-NAME")
    <*> (optional $ argument str $ metavar "REF")
    <*> quietOption

parseDeleteBranch :: Parser Command
parseDeleteBranch = DeleteBranch
    <$> (argument str $ metavar "BRANCH-NAME")
    <*> quietOption

parseListBranches :: Parser Command
parseListBranches = ListBranches <$> quietOption

parseRetrack :: Parser Command
parseRetrack = Retrack <$> (argument str $ metavar "PATH")

parseDiff :: Parser Command
parseDiff = Diff <$> quietOption

parseBranch :: Parser Command
parseBranch = subparser
    $  command "create" (parseCreateBranch `withInfo` branchCreateHelpMessage)
    <> command "delete" (parseDeleteBranch `withInfo` branchDeleteHelpMessage)
    <> command "list"   (parseListBranches `withInfo` branchListHelpMessage)
    where
        branchCreateHelpMessage :: String
        branchCreateHelpMessage = "Create a branch"

        branchDeleteHelpMessage :: String
        branchDeleteHelpMessage = "Delete a branch"

        branchListHelpMessage :: String
        branchListHelpMessage = "List all branches"

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
    <> command "branch"   (parseBranch   `withInfo` branchHelpMessage)
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

        branchHelpMessage :: String
        branchHelpMessage = "Perform branch operations"

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- abbreviations:
--     m - message
--     p - printer
--     r - ref
--dispatchCommand
dispatchCommand cmd = runEitherT $ case cmd of
    Version            -> liftIO . putStrLn $ "0.1.0.0"
    Init p             -> Commands.init                      p
    Checkout r p       -> Commands.checkout r                p
    Config name email  -> void $ Commands.config name email
    Show r p           -> void $ Commands.show r             p
    Stage path         -> void $ Commands.stage path
    Unstage path       -> void $ Commands.unstage path
    Log r n p          -> void $ Commands.log r n            p
    Status p           -> void $ Commands.status             p
    Commit m False p   -> void $ Commands.commit def m       p
    Commit m True p    -> void $ Commands.commitAmend def m  p
    Squash r           -> void $ Commands.squash def r
    Retrack path       -> void $ Commands.retrack path
    Diff p             -> void $ Commands.diff               p
    CreateBranch b r p -> void $ Commands.createBranch b r   p
    DeleteBranch b p   -> void $ Commands.deleteBranch b     p
    ListBranches p     -> void $ Commands.listBranches       p
    Untrack _ True p   -> void $ Commands.listUntrackedPaths p
    Untrack (Just path) False p -> void $ Commands.untrack path p

run :: Command -> IO ()
run cmd = do
    eitherSuccess <- dispatchCommand cmd
    if isLeft eitherSuccess
        then putStrLn $ fromLeft def eitherSuccess
        else return ()

main :: IO ()
main = run =<< execParser
    (parseCommand `withInfo` "Version control, written in Haskell with all that monadic goodness!")
