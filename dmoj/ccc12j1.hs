main = do
    inp <- getLine
    lim <- return (read inp)
    inp <- getLine
    spd <- return (read inp)
    if spd <= lim then
        putStrLn "Congratulations, you are within the speed limit!"
    else if spd <= lim + 20 then
        putStrLn "You are speeding and your fine is $100."
    else if spd <= lim + 30 then
        putStrLn "You are speeding and your fine is $270."
    else
        putStrLn "You are speeding and your fine is $500."
