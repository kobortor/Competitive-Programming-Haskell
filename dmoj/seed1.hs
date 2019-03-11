(!=) x y = not (x == y)

main = do
    parts <- getLine
    putStrLn (solve parts "BFTLC")
    where
        solve "" str    = if str == "" then "NO MISSING PARTS" else str
        solve inp str   = solve (tail inp) (filter ((!=) (head inp)) str)
