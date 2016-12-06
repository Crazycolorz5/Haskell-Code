fizzBuzz::Int->String
fizzBuzz x
    | mod x 15 == 0 = "fizz buzz"
    | mod x 5 == 0  = "buzz"
    | mod x 3 == 0  = "fizz"
    | otherwise     = show x

main = putStrLn "Up to what number do you want to fizzBuzz?" >> getLine >>= \x->mapM_ (putStrLn . fizzBuzz) [1..(read x)]