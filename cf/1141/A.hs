solve3 n m res
    | mod m (n * 3) == 0    = solve3 (n * 3) m (res + 1)
    | n == m                = res
    | otherwise             = -1

solve2 n m res
    | mod m (n * 2) == 0    = solve2 (n * 2) m (res + 1)
    | otherwise             = solve3 n m res

main = do
    n:m:[] <- (map read :: [String] -> [Int]) <$> words <$> getLine
    putStrLn (show (solve2 n m 0))
