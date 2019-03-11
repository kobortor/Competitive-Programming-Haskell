main = do
    nstr <- getLine
    n <- return (read nstr)
    loop n
    where
        loop k
            | k == 0    = putStrLn ""
            | otherwise = do
                inp <- getLine
                nums <- return (words inp)
                a <- return (read (head nums) :: Int)
                b <- return (read (head (tail nums)) :: Int)
                putStrLn (show (a + b))
                loop (k - 1)

