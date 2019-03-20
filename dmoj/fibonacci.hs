data Mat22 = Mat22 Integer Integer Integer Integer

mult :: Mat22 -> Mat22 -> Mat22
mult (Mat22 a11 a12 a21 a22) (Mat22 b11 b12 b21 b22) =
    Mat22   (mod (a11 * b11 + a12 * b21) m) (mod (a11 * b12 + a12 * b22) m)
            (mod (a21 * b11 + a22 * b21) m) (mod (a21 * b12 + a22 * b22) m)
    where m = 1000000007 :: Integer

expon :: Mat22 -> Integer -> Mat22
expon m p
    | p == 0            = Mat22 1 0 0 1
    | (mod p 2) == 0    = sqr (expon m (quot p 2))
    | otherwise         = mult m (sqr (expon m (quot p 2)))
    where sqr x = mult x x

main = do
    n <- getLine
    n <- return (read n :: Integer)
    (Mat22 _ x _ _) <- return (expon (Mat22 1 1 1 0) n)
    putStrLn (show x)
