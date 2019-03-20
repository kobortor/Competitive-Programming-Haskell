import Data.List

main = do
    inp <- getLine
    (n:k:xs) <- return (map read (words inp) :: [Int])
    arr <- (readn n [])
    putStrLn (show (foldl (+) 0 (take k (reverse (sort arr)))))
    where
        readn :: Int -> [Int] -> IO [Int]
        readn 0 res = return res
        readn n res = do
            str <- getLine
            if (read str) >= 0
            then readn (n - 1) ((read str) : res)
            else readn (n - 1) res
