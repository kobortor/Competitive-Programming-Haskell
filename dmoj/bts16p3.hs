import Data.Char

solve [] = 0
solve ls@(x:xs) = (cnt $ length lft) + solve rht
    where   (lft, rht) = span (x ==) ls
            cnt n = quot (n * (n + 1)) 2

main = do
    n <- read <$> getLine :: IO Int
    lst <- map (toLower . head) <$> words <$> getLine
    putStrLn $ show $ solve lst
