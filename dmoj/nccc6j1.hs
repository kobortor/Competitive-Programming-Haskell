main = do
    a <- read <$> getLine :: IO Int
    b <- read <$> getLine :: IO Int
    case compare a b of
        LT -> putStrLn "PHIL145"
        GT -> putStrLn "CS452"
        EQ -> return ()
