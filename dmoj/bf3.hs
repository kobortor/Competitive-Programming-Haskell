is_prime 1 = False
is_prime n = go n 2
    where go n chk
            | chk * chk > n     = True
            | mod n chk == 0    = False
            | otherwise         = go n (chk + 1)

solve n
    | is_prime n    = n
    | otherwise     = solve (n + 1)

main = do
    n <- read <$> getLine :: IO Int
    putStrLn (show (solve n))

