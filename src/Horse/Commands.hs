module Horse.Commands
( -- * Basic commands
  Horse.Commands.add
, Horse.Commands.rm
, Horse.Commands.checkout
, Horse.Commands.commit
, Horse.Commands.diff
, Horse.Commands.init
, Horse.Commands.log
, Horse.Commands.status
, Horse.Commands.show
) where

import Prelude
import qualified Prelude (show, init, log)

add :: [String] -> IO ()
add args = do
    putStrLn $ "running command \"add\" with args " ++ Prelude.show args

rm :: [String] -> IO ()
rm args = do
    putStrLn $ "running command \"rm\" with args " ++ Prelude.show args

checkout :: [String] -> IO ()
checkout args = do
    putStrLn $ "running command \"checkout\" with args " ++ Prelude.show args

commit :: [String] -> IO ()
commit args = do
    putStrLn $ "running command \"commit\" with args " ++ Prelude.show args

diff :: [String] -> IO ()
diff args = do
    putStrLn $ "running command \"diff\" with args " ++ Prelude.show args

init :: [String] -> IO ()
init args = do
    putStrLn $ "running command \"init\" with args " ++ Prelude.show args

log :: [String] -> IO ()
log args = do
    putStrLn $ "running command \"log\" with args " ++ Prelude.show args

status :: [String] -> IO ()
status args = do
    putStrLn $ "running command \"status\" with args " ++ Prelude.show args

show :: [String] -> IO ()
show args = do
    putStrLn $ "running command \"show\" with args " ++ Prelude.show args
