import Data.Char

solve str = case compare costlow costcap of
    LT -> map toLower str
    GT -> map toUpper str
    EQ -> str
    where
        costlow = length (filter (\x -> x /= toLower x) str)
        costcap = length (filter (\x -> x /= toUpper x) str)

main = do
    str <- getLine
    putStrLn (solve str)
