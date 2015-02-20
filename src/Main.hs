import System.Environment
import System.Exit

import qualified Horse.Commands as Commands

helpText :: String
helpText = "<insert help text here>"

version :: String
version = "v0.1.0.0"

parseArgs :: [String] -> IO String
parseArgs [] = parseArgs ["-h"]
parseArgs args =
    case head args of
        "add"           -> (Commands.add         $ tail args ) >> exit
        "rm"            -> (Commands.rm          $ tail args ) >> exit
        "checkout"      -> (Commands.checkout    $ tail args ) >> exit
        "commit"        -> (Commands.commit      $ tail args ) >> exit
        "diff"          -> (Commands.diff        $ tail args ) >> exit
        "init"          -> (Commands.init        $ tail args ) >> exit
        "log"           -> (Commands.log         $ tail args ) >> exit
        "status"        -> (Commands.status      $ tail args ) >> exit
        "show"          -> (Commands.show        $ tail args ) >> exit
        "-v"            -> (putStrLn version                 ) >> exit
        "-h"            -> (putStrLn helpText                ) >> exit
        _               -> ((putStrLn $ "Unrecognized command: "
                                         ++ (head args))     ) >> die
    where
        exit = exitWith ExitSuccess
        die  = exitWith (ExitFailure 1)

main :: IO ()
main = getArgs >>= parseArgs >>= putStr
