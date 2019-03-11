oddeven lst = go lst [] []
    where
        go [] odds evens = (odds, evens)
        go (x:xs) odds evens = go xs evens (x : odds)

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort (x:[]) = [x]
mergesort lst =
    merge (mergesort odd) (mergesort even) []
    where
        (odd, even) = oddeven lst
        merge [] [] ls = reverse ls
        merge [] (e:es) ls = merge [] es (e : ls)
        merge (o:os) [] ls = merge os [] (o : ls)
        merge (o:os) (e:es) ls
            | o < e     = merge os (e:es) (o:ls)
            | otherwise = merge (o:os) es (e:ls)

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
    sorted <- return (mergesort lst)
    printlist sorted
