module Import
    ( module X
    , module Import
    ) where

import Control.Concurrent as X
import Control.Monad as X
import System.Exit as X (ExitCode (..))
import System.Environment as X
import System.Posix.Signals as X
import System.Posix.Process as X
import System.Process as X
import System.IO as X
import qualified Data.ByteString.Char8 as S8

say :: String -> IO ()
say s = do
    S8.putStr (S8.pack (s ++ "\n"))
    hFlush stdout

die :: IO ()
die = exitImmediately (ExitFailure 42)
