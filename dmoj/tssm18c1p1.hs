import Data.List

wordx :: String -> [String]
wordx str = go str ""
    where
        go [] res = [res]
        go ('x':str) res = res : go str ""
        go (x:xs) res = go xs (x : res)

main = do 
    lst <- sort <$> map read <$> wordx <$> getLine :: IO [Int]
    putStrLn $ intercalate "x" $ map show lst
    putStrLn $ show $ foldl1 (*) lst
