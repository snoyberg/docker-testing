import Control.Concurrent
import Control.Monad
import System.Environment
import System.Exit (ExitCode (..))
import System.Posix.Signals
import System.Posix.Process
import System.Process
import System.IO
import qualified Data.ByteString.Char8 as S8

say :: String -> IO ()
say s = do
    S8.putStr (S8.pack (s ++ "\n"))
    hFlush stdout

die :: String -> IO ()
die msg = do
    say msg
    exitImmediately (ExitFailure 42)

child :: Int -> IO ()
child i = do
    _ <- createProcess (proc "surviving" [show i])
    return ()

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    _ <- installHandler sigTERM (Catch (die "Got a TERM")) Nothing

    args <- getArgs
    case args of
        [] -> do
            mapM_ child [1..4]

            say "Parent sleeping"
            threadDelay 2000000
            say "Parent exiting"
        [num] -> forever $ do
            say ("Child: " ++ num)
            threadDelay 1000000
