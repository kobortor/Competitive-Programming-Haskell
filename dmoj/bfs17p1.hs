main = do
    n <- getLine
    ans <- length <$> filter ((>=) 10) <$> map length <$> words <$> getLine
    putStrLn (show ans)
