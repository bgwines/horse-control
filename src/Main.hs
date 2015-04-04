{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

import System.Environment
import System.Exit

import qualified Horse.Types as Types
import qualified Horse.Commands.Porcelain as Porcelain

import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.IO.Class (liftIO)

import Data.Either.Combinators (isLeft, fromLeft)

import Options.Applicative

import Data.Default as Default

data Command
    = Init
    | Version
    | Config (Maybe String) (Maybe Types.Email)
    | Status
    | Stage String
    | Commit (Maybe String)
    | Checkout String
    | Show (Maybe String)
    | Log (Maybe String) (Maybe Int)
    deriving (Show)

parseInit :: Parser Command
parseInit = pure Init

parseStatus :: Parser Command
parseStatus = pure Status

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

parseStage :: Parser Command
parseStage = Stage <$> (argument str $ metavar "FILE-OR-DIRECTORY")

parseCommit :: Parser Command
parseCommit = Commit
    <$> (optional $ strOption
            ( short 'm'
            <> metavar "COMMIT-MESSAGE" )
        )

parseCheckout :: Parser Command
parseCheckout = Checkout <$> (argument str $ metavar "REF")

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

parseVersion :: Parser Command
parseVersion = pure Version

parseCommand :: Parser Command
parseCommand = subparser
    $  command "init"     (parseInit     `withInfo` initHelpMessage)
    <> command "config"   (parseConfig   `withInfo` configHelpMessage)
    <> command "status"   (parseStatus   `withInfo` statusHelpMessage)
    <> command "stage"    (parseStage    `withInfo` stageHelpMessage)
    <> command "commit"   (parseCommit   `withInfo` commitHelpMessage)
    <> command "checkout" (parseCheckout `withInfo` checkoutHelpMessage)
    <> command "show"     (parseShow     `withInfo` showHelpMessage)
    <> command "log"      (parseLog      `withInfo` logHelpMessage)
    <> command "version"  (parseVersion  `withInfo` "Show version")
    where
        initHelpMessage :: String
        initHelpMessage = "Initialize an empty repository"

        configHelpMessage :: String
        configHelpMessage = "Configure horse-control with custom settings"

        statusHelpMessage :: String
        statusHelpMessage = "Get information about the relationship between the current working directory and the current staging area"

        stageHelpMessage :: String
        stageHelpMessage = "Stage additions, modifications, or deletions to files."

        commitHelpMessage :: String
        commitHelpMessage = "Write the staging area as a commit"

        checkoutHelpMessage :: String
        checkoutHelpMessage = "Set HEAD to a ref and adjust the working directory accordingly"

        showHelpMessage :: String
        showHelpMessage = "Print the specified ref"

        logHelpMessage :: String
        logHelpMessage = "Print the commit history"

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

run :: Command -> IO ()
run cmd = do
    eitherSuccess <- case cmd of
        Version            -> fmap Right $ putStrLn "0.1.0.2"
        Init               -> fmap Right $ Porcelain.init
        Config name email  -> fmap Right $ Porcelain.config name email
        Status             -> runEitherT $ Porcelain.status
        Stage path         -> runEitherT $ Porcelain.stage path
        Commit message     -> runEitherT $ void $ Porcelain.commit message
        Checkout ref       -> runEitherT $ Porcelain.checkout ref
        Show ref           -> runEitherT $ Porcelain.hshow ref
        Log ref n          -> runEitherT $ Porcelain.log ref n
    if isLeft eitherSuccess
        then putStrLn $ fromLeft Default.def eitherSuccess
        else return ()

main :: IO ()
main = run =<< execParser
    (parseCommand `withInfo` "Version control, written in Haskell with all that monadic goodness!")
