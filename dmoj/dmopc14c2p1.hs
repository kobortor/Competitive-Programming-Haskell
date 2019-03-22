import Control.Monad

main = do
    d <- getLine
    go 1 (read d)
    where
        go n d
            | n > d     = return ()
            | otherwise = do
                l <- getLine
                if l == "0" then
                    putStrLn "Weekend"
                else do
                    lst <- replicateM (read l) getLine
                    putStrLn ("Day " ++ (show n) ++ ": " ++ (show (foldl1 (+) (map read lst :: [Int]))))
                go (n + 1) d
