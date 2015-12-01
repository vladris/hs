coprime :: Int -> Int -> Bool
coprime x y = gcd x y == 1

main = do
    print(coprime 35 64)
