main = do
    n <- getLine
    n <- return (read n :: Int)
    a1 <- getLine
    a2 <- getLine
    a1 <- return (map read (words a1) :: [Int])
    a2 <- return (map read (words a2) :: [Int])
    putStrLn (show (solve 0 0 0 a1 a2))
    where
        solve :: Int -> Int -> Int -> [Int] -> [Int] -> Int
        solve sum idx ans [] [] = ans
        solve sum idx ans (a:as) (b:bs)
            | sum + a - b == 0  = solve (sum + a - b) (idx + 1) (idx + 1) as bs
            | otherwise         = solve (sum + a - b) (idx + 1) ans as bs

