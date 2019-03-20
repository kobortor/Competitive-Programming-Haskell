main = do
    n <- getLine
    go (read n)
    where
        go 0 = return ()
        go n = do
            inp <- getLine
            putStrLn (getname (read inp))
            go (n - 1)
        getname inp
            | inp < 1000    = "Newbie"
            | inp < 1200    = "Amateur"
            | inp < 1500    = "Expert"
            | inp < 1800    = "Candidate Master"
            | inp < 2200    = "Master"
            | inp < 3000    = "Grandmaster"
            | inp < 4000    = "Target"
            | otherwise     = "Rainbow Master"
