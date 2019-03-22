import Control.Monad 

main = do
    format <- getLine
    n <- getLine
    lst <- replicateM (read n) getLine
    go lst format
    where
        go [] format = return ()
        go (n:ns) format = do
            putStrLn (foldr1 (++) (map (\ c -> if c == '>' then n else c : []) format))
            go ns format
