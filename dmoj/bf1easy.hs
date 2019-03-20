main = do
    nstr <- getLine
    n <- return (read nstr :: Int)
    go 1 n
    where 
        go m k
            | m > k     = return ()
            | otherwise = do
                putStr (show m)
                putStr " "
                go (m + 1) k
