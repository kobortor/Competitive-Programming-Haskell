main = do
    n <- getLine
    go (read n) 0
    where
        go 0 res = putStrLn (show res)
        go n res = do
            name <- getLine
            if or [
                (and $ map (\ x -> elem x top) name),
                (and $ map (\ x -> elem x mid) name),
                (and $ map (\ x -> elem x bot) name)]
            then go (n - 1) (res + 1)
            else go (n - 1) res
        top = "qwertyuiop"
        mid = "asdfghjkl"
        bot = "zxcvbnm"

