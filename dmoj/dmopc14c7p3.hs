import Data.List

main = do
    n <- getLine
    getLine -- ignore this
    l <- getLine
    r <- getLine
    putStrLn (show 
        ((read n) - 
        (go
            (sort (map read (words l) :: [Int]))
            (sort (map read (words r) :: [Int])))))
    where
        go x [] = 0
        go [] y = 0
        go xx@(x:xs) yy@(y:ys)
            | x == y    = 1 + (go xs ys)
            | x < y     = go xs yy 
            | otherwise = go xx ys
