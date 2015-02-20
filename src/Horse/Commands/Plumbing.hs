module Horse.Commands.Plumbing
( -- * Basic commands
  hashObject
) where

import qualified System.IO as IO
import qualified System.Directory as Dir

import qualified Horse.Constants as Constants

{-
~/.horseconfig : { name :: String
                 , email :: String }

./.horse
    info : { head :: Hash
           , stagingArea :: [diffhash] }

    diffs
        hash => (FilePath, diff)

    commits
        hash => ( author -- (name, e-mail)
                , date
                , hash
                , [parenthash]
                , [diffhash] ) -- TODO: tree?
-}

hashObject :: String -> IO ()
hashObject s = return ()