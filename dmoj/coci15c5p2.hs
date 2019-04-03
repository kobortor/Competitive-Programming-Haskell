import Data.List

all_same (x:[]) = True
all_same (x:y:xs) = x == y && all_same (y:xs)

split [] len = []
split str len = (sort $ take len str) : split (drop len str) len

solve str len chk
    | len == chk            = show (-1)
    | len `mod` chk == 0    = if all_same $ split str chk then take chk str else solve str len (chk + 1)
    | otherwise             = solve str len (chk + 1)

main = do
    (str, len) <- (\x -> (x, length x)) <$> getLine
    putStrLn $ solve str len 1
