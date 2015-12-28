primes = [i | i <- [2..10^6], null [j | j <- [2..round(sqrt(fromIntegral i))], i `mod` j == 0]]

prime_factors n = [i | i <- primes, n `mod` i == 0]

result n = head [i | i <- [1..], all (>=n) [length (prime_factors x)| x <- [i..i + n - 1]]]

main = do
    print(result 4)
