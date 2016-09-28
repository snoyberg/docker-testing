import Import

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
