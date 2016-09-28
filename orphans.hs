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

die :: IO ()
die = exitImmediately (ExitFailure 42)

echo :: Int -> IO ()
echo i = do
    _ <- createProcess (proc "echo" [show i])
    return ()

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    args <- getArgs
    case args of
        [] -> do
            _ <- forkIO (callProcess "orphans" ["child"])

            forever $ do
                threadDelay 1000000
                say "Still alive!"
                callProcess "ps" []
        ["child"] -> do
            mapM_ echo [1..4]

            threadDelay 2000000
