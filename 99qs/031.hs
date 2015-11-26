isPrimeHelper n i
    | i > floor(sqrt(fromInteger n)) = True
    | n `mod` i == 0 = False
    | otherwise = isPrimeHelper n (i + 1)

isPrime n = isPrimeHelper n 2

main = do
    print(isPrime 7)
