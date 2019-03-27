import Data.List
import Data.Char

solve :: [String] -> [String]
solve (x1:x2:xs) = if isUpper (head x2) then (x1 ++ ".") : (solve (x2 : xs)) else x1 : (solve (x2 : xs))
solve (x:_) = ((x ++ ".") : [])

main = do
    lst <- words <$> getLine
    putStrLn $ intercalate " " (solve lst)
