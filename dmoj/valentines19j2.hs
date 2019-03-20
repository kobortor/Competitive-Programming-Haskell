main = do
    n <- getLine
    go (read n) 0
    where
        go 0 res = putStrLn (show res)
        go n res = do
            inp <- getLine
            r:g:b:xs <- return (map read (words inp) :: [Int])
            if 240 <= r && g <= 200 && 95 <= b && b <= 220
            then go (n - 1) (res + 1)
            else go (n - 1) res
