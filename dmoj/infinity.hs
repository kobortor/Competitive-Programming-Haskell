import Data.Char

main = do
    a <- getLine
    b <- getLine
    if (parsehex a) + (parsehex b) > (parsehex "3F3F3F3F")
    then putStrLn "Yes"
    else putStrLn "No"

chartonum ch
    | ord '0' <= od && od <= ord '9'    = od - (ord '0')
    | otherwise                         = od - (ord 'A') + 10
    where od = ord (toUpper ch)

parsehex str = go str 0
    where
        go "" res   = res
        go str res  = go (tail str) (res * 16 + (chartonum (head str)))
