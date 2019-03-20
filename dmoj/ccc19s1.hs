main = do
    str <- getLine
    solve str [[1, 2], [3, 4]]
    where
        solve st grid 
            | st == ""          = do
                putStrLn ((show v1) ++ " " ++ (show v2))
                putStrLn ((show v3) ++ " " ++ (show v4))
            | (head st) == 'H'  = solve (tail st) [[v3, v4], [v1, v2]]
            | (head st) == 'V'  = solve (tail st) [[v2, v1], [v4, v3]]
            where
                v1 = head (head grid)
                v2 = head (tail (head grid))
                v3 = head (head (tail grid))
                v4 = head (tail (head (tail grid)))
