import Control.Monad

solve 5 = 1
solve 6 = 1

solve 3 = 2
solve 4 = 2

solve 1 = 3
solve 2 = 3

solve 0 = -1

main = do
    val <- length <$> filter (== "W") <$> replicateM 6 getLine
    putStrLn $ show $ solve val
