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

die :: String -> Signal -> IO ()
die name val = do
    say ("Killed by " ++ name)
    --exitImmediately (ExitFailure (fromIntegral val + 128))

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    let signals =
            [ ("TERM", sigTERM)
            , ("INT", sigINT)
            ]

    forM_ signals $ \(name, val) ->
        void (installHandler val (Catch (die name val)) Nothing)
    forever $ do
        say "Still alive!"
        threadDelay 1000000
