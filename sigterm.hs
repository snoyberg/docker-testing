import Control.Concurrent
import Control.Monad
import System.Exit (ExitCode (..))
import System.Environment
import System.Posix.Signals
import System.Posix.Process
import System.Process
import System.IO
import qualified Data.ByteString.Char8 as S8

say :: String -> IO ()
say s = do
    S8.putStr (S8.pack (s ++ "\n"))
    hFlush stdout

die :: IO ()
die = exitImmediately (ExitFailure 42)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    callProcess "ps" []

    args <- getArgs
    when (args == ["install-handler"])
         (void (installHandler sigTERM (Catch die) Nothing))
    raiseSignal sigTERM
    forever $ do
        say "Still alive!"
        threadDelay 1000000
