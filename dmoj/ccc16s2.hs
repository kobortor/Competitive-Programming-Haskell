import Data.List

solve1 [] [] = 0
solve1 dmoj peg = foldl1 (+) $ zipWith max dmoj peg

solve2 [] [] = 0
solve2 dmoj peg = 
    if mx1 > mx2
    then mx1 + solve2 (init dmoj) (tail peg)
    else mx2 + solve2 (tail dmoj) (init peg)
    where
        mx1 = maximum dmoj
        mx2 = maximum peg 

main = do
    tp <- read <$> getLine :: IO Int
    n <- read <$> getLine :: IO Int
    dmoj <- sort <$> map read <$> words <$> getLine :: IO [Int]
    peg <- sort <$> map read <$> words <$> getLine :: IO [Int]
    putStrLn $ show $ (if tp == 1 then solve1 else solve2) dmoj peg
