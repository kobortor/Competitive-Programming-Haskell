main = do
    str <- getLine
    putStrLn (show (go str 0))
    where
        go "" idx = idx
        go st idx = if ((take 4 st) == "java") then idx else go (tail st) (idx + 1)
