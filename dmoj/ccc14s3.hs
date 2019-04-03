import Control.Monad

solve [] [] nxt = True
solve lx ly nxt
    | not (null lx) && (head lx) == nxt = solve (tail lx) ly (nxt + 1)
    | not (null ly) && (head ly) == nxt = solve lx (tail ly) (nxt + 1)
    | null lx                           = False
    | otherwise                         = solve (tail lx) ((head lx) : ly) nxt

testcase = do
    n <- read <$> getLine :: IO Int
    lst <- replicateM n getLine
    putStrLn $ if solve (reverse $ map read $ lst :: [Int]) [] 1 then "Y" else "N"

main = do
    t <- read <$> getLine :: IO Int
    replicateM t testcase
