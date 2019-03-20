import Data.Char

data Mat22 = Mat22 !Int !Int !Int !Int
ident = Mat22 1 0 0 1
fib = Mat22 1 1 1 0

mult :: Mat22 -> Mat22 -> Mat22
mult (Mat22 a11 a12 a21 a22) (Mat22 b11 b12 b21 b22) =
    Mat22   (mod (a11 * b11 + a12 * b21) m) (mod (a11 * b12 + a12 * b22) m)
            (mod (a21 * b11 + a22 * b21) m) (mod (a21 * b12 + a22 * b22) m)
    where m = 1000000007 :: Int

exp10 m0 = mult m1 m3
    where   m1 = mult m0 m0 
            m2 = mult m1 m1
            m3 = mult m2 m2

small_exp :: Mat22 -> Int -> Mat22
small_exp m p = foldl mult ident (take p (repeat m))

expon :: Mat22 -> String -> Mat22 -> Mat22
expon m "" res = res
expon m (s:ss) res = expon (exp10 m) ss (mult (small_exp m ((ord s) - (ord '0'))) res)

main = do
    n <- getLine
    (Mat22 _ x _ _) <- return (expon fib (reverse n) (Mat22 1 0 0 1))
    putStrLn (show x)
