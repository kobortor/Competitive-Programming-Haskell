solve str = go str (0, 0, 0, 0)
    where
        go [] (l, o, v, e) = e
        go ('l' : str) (l, o, v, e) = go str (l + 1, o, v, e)
        go ('o' : str) (l, o, v, e) = go str (l, o + l, v, e)
        go ('v' : str) (l, o, v, e) = go str (l, o, v + o, e)
        go ('e' : str) (l, o, v, e) = go str (l, o, v, e + v)
        go (_ : str) tup = go str tup

main = do
    str <- getLine
    putStrLn $ show $ solve str
