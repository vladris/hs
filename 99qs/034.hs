totient :: Int -> Int
totient x = length [t | t <- [1..x], gcd x t == 1]

main = do
    print(totient 10)
