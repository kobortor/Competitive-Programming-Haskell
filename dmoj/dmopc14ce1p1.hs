main = do
    verb <- getLine
    subject <- getLine
    putStrLn (case drop ((length subject) - 1) subject of
        ('e':[])  -> (verb ++ "-tu la " ++ subject ++ " ?")
        ('s':[])  -> verb ++ "-tu les " ++ subject ++ " ?"
        x       -> verb ++ "-tu le " ++ subject ++ " ?")

