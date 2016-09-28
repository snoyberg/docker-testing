import Import

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
        _ -> error $ "Unknown args: " ++ show args
