import Data.List
import Data.Char

solve 0 set = return ()
solve n set = do
    inp <- getLine
    let args = words inp
        val = head (tail args)
        in case read $ head args of
        1 -> case find (val ==) set of
                Nothing -> do
                    putStrLn "true"
                    solve (n - 1) (sort (val : set))
                Just x  -> do
                    putStrLn "false"
                    solve (n - 1) set
        2 -> case find (val ==) set of
                Nothing -> do
                    putStrLn "false"
                    solve (n - 1) set
                Just x  -> do
                    putStrLn "true"
                    solve (n - 1) (filter (val /=) set)
        3 -> case find (val ==) set of
                Nothing -> do
                    putStrLn "-1"
                    solve (n - 1) set
                Just x  -> do
                    putStrLn (show (length (filter (val >) set)))
                    solve (n - 1) set
        4 -> do
            putStrLn (map toLower (intercalate " " set))
            solve (n - 1) set

main = do
    n <- read <$> getLine :: IO Int
    solve n []

