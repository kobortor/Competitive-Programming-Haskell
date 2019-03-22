import Data.List
import Control.Monad

main = do
    n <- getLine
    t <- getLine
    pumpkins <- replicateM (read n) getLine
    lst <- return (sort (map read pumpkins :: [Int]))
    putStrLn (show (solve lst (drop ((read t) - 1) lst)))
    where 
        solve lst [] = maxBound :: Int
        solve (x:xs) (y:ys) 
            | y < 0     = (let res = -x in min res (solve xs ys))
            | x > 0     = (let res = y in min res (solve xs ys))
            | otherwise = (let res = y - x + (min y (0 - x)) in min res (solve xs ys))
