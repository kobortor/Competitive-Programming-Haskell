solve str k = go str k (repeat 'z')
    where go str k res
            | length tmp < k    = res
            | otherwise         = go (tail str) k (min res tmp)
            where tmp = take k str

main = do
    str <- getLine
    k <- read <$> getLine :: IO Int
    putStrLn (solve str k)
