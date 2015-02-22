{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

import System.Environment
import System.Exit

import qualified Horse.Commands.Porcelain as Porcelain
import qualified Horse.Commands.Plumbing as Plumbing
import qualified Horse.Filesys as Filesys
import qualified Horse.Types as Types

helpText :: String
helpText = "<insert help text here>"

version :: String
version = "v0.1.0.0"

parseArgs :: [String] -> IO String
parseArgs [] = parseArgs ["-h"]
parseArgs args =
    case head args of
        "add"           -> (Porcelain.add        $ tail args ) >> exit
        "rm"            -> (Porcelain.rm         $ tail args ) >> exit
        "checkout"      -> (Porcelain.checkout   $ tail args ) >> exit
        "commit"        -> (Porcelain.commit     $ tail args ) >> exit
        "diff"          -> (Porcelain.diff       $ tail args ) >> exit
        "init"          -> (Porcelain.init       $ tail args ) >> exit
        "log"           -> (Porcelain.log        $ tail args ) >> exit
        "status"        -> (Porcelain.status     $ tail args ) >> exit
        "show"          -> (Porcelain.show       $ tail args ) >> exit
        "-v"            -> (putStrLn version                 ) >> exit
        "-h"            -> (putStrLn helpText                ) >> exit
        _               -> ((putStrLn $ "Unrecognized command: "
                                         ++ (head args))     ) >> die
    where
        exit = exitWith ExitSuccess
        die  = exitWith (ExitFailure 1)

main :: IO ()
main = do getArgs >>= parseArgs >>= putStr
