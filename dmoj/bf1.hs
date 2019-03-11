qsort :: [Int] -> [Int]
qsort [] = []
qsort (x : xs) = 
    (qsort lft) ++ [x] ++ (qsort rht)
    where
        lft = filter ((>=) x) xs
        rht = filter ((<) x) xs

readlist 0 = return []
readlist n = do
    nstr <- getLine 
    nxt <- readlist (n - 1)
    return ((read nstr :: Int) : nxt)

printlist [] = return ()
printlist (x:xs) = do
    putStrLn (show x)
    printlist xs

main = do
    nstr <- getLine
    n <- return(read nstr :: Int)
    lst <- readlist n 
    sorted <- return (qsort lst)
    printlist sorted
