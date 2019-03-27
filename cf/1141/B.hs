solve [] res cur = max res cur
solve (0:xs) res cur = solve xs (max res cur) 0
solve (1:xs) res cur = solve xs res (cur + 1)

main = do
    n <- (read :: String -> Int) <$> getLine
    lst <- (map read :: [String] -> [Int]) <$> words <$> getLine
    putStrLn (show (min n (solve (lst ++ lst) 0 0)))
