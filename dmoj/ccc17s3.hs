import Data.List

joinadj [] out = out
joinadj (l:ls) [] = joinadj ls [(l, 1)]
joinadj (l:ls) out@((x,y) : xys)
    | l == x    = joinadj ls ((x, y + 1) : xys)
    | otherwise = joinadj ls ((l, 1) : out)

main = do
    n <- getLine
    n <- return (read n :: Int)
    arr <- getLine
    arr <- return (sort (map read (words arr) :: [Int]))
    freq <- return (joinadj arr [])
    go freq (reverse freq) 4000 []
    where
        go fwd rev 0 ans = 
        go fwd rev tot ans
