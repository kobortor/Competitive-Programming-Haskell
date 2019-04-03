import Data.List 
import Control.Monad

solve lst [] = lst
solve lst (r:rs) = solve (go lst r 0) rs
    where
        go [] r c = []
        go (l:ls) r c = if r == c + 1 then go ls r 0 else l : (go ls r (c + 1))

main = do
    k <- read <$> getLine
    m <- read <$> getLine
    rmv <- map read <$> replicateM m getLine
    putStr $ intercalate "\n" $ map (show :: Int -> String) $ solve [1..k] rmv
    
