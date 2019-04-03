import Control.Monad
import Data.List

rowsumeq mat = and $ (\(x:xs) -> map (== x) xs) $ map sum mat

main = do
    mat <- (map $ (map read) . words) <$> (replicateM 4 getLine)
    putStrLn $ if rowsumeq mat && rowsumeq (transpose mat) then "magic" else "not magic"
