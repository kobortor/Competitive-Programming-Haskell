import Data.List

join_lst [] [] = []
join_lst (x:xs) (y:ys) = ((min x y), (max x y)) : join_lst xs ys

check [] = True
check (x1:x2:xs) = if x1 == x2 then check xs else False

main = do
    n <- read <$> getLine :: IO Int
    lst <- join_lst <$> (words <$> getLine) <*> (words <$> getLine)
    if mod n 2 == 1 then
        putStrLn "bad"
    else if check (sort lst) then
        putStrLn "good"
    else
        putStrLn "bad"
        
    
