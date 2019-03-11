solve 0 first = putStrLn "YES"
solve n first = do
    str <- getLine
    if (read str :: Int) >= first
    then putStrLn "NO"
    else solve (n - 1) first
    

main = do
    nstr <- getLine
    n <- return (read nstr :: Int)
    first <- getLine
    solve (n - 1) (read first :: Int)
