substrings [] = []
substrings str = (go str) ++ (substrings $ init str)
    where   go [] = []
            go str = str : (go $ tail str)

main = do
    str <- getLine
    putStrLn $ show $ maximum $ map length $ filter (\x -> x == reverse x) $ substrings str
