main = do
    inp <- getLine
    lst <- return (words inp)
    a <- return (read (head lst) :: Int)
    b <- return (read (head (tail lst)) :: Int)
    c <- return (read (head (tail (tail lst))) :: Int)
    putStrLn (solve a b c)
    where
        solve x y z
            | x + y == z    = (show x) ++ "+" ++ (show y) ++ "=" ++ (show z)
            | x - y == z    = (show x) ++ "-" ++ (show y) ++ "=" ++ (show z)
            | x * y == z    = (show x) ++ "*" ++ (show y) ++ "=" ++ (show z)
            | x == y + z    = (show x) ++ "=" ++ (show y) ++ "+" ++ (show z)
            | x == y - z    = (show x) ++ "=" ++ (show y) ++ "-" ++ (show z)
            | x == y * z    = (show x) ++ "=" ++ (show y) ++ "*" ++ (show z)
            | x * z == y    = (show x) ++ "=" ++ (show y) ++ "/" ++ (show z)

