import Data.List

mp "red" = 0
mp "orange" = 1
mp "yellow" = 2
mp "green" = 3
mp "blue" = 4
mp "black" = 5

mx_color col res cur [] = res
mx_color 6 res cur ls = res
mx_color col res cur xx@(x:xs)
    | col == x  = mx_color col (max res (cur + 1)) (cur + 1) xs
    | otherwise = mx_color (col + 1) (max res cur) 0 xx

main = do
    n <- read <$> getLine :: IO Int
    mx <- mx_color 0 0 0 <$> sort <$> map mp <$> words <$> getLine
    putStrLn (show (min n (2 * (n - mx) + 1)))
