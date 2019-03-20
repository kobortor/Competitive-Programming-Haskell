import Data.List

main = do
    n <- getLine
    n <- return (read n :: Int)
    arr <- getLine
    arr <- return (sort (map read (words arr) :: [Int]))
    low_lens <- return (quot (n + 1) 2)
    lows <- return (reverse (take low_lens arr))
    highs <- return (drop low_lens arr)
    go lows highs
    where
        go [] [] = return ()
        go (x:xs) y = do
            putStr (show x)
            putStr " "
            go y xs
