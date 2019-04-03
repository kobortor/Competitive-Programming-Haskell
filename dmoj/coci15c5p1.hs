digsum 0 = 0
digsum n = (n `rem` 10) + digsum (n `quot` 10)

main = do
    l <- read <$> getLine :: IO Int
    d <- read <$> getLine :: IO Int
    x <- read <$> getLine :: IO Int
    lst <- return $ filter (\y -> digsum y == x) [l..d]
    putStrLn $ show $ minimum lst
    putStrLn $ show $ maximum lst
