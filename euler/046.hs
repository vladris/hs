primes = [i | i <- [2..10^6], null [j | j <- [2..round(sqrt(fromIntegral i))], i `mod` j == 0]]

conjecture n = (not . null) [i | i <- takeWhile (<n) primes, let x = ((n - i) `div` 2) in x == round(sqrt(fromIntegral x))^2]

result = head [i | i <- [3,5..], not (elem i primes), (not . conjecture) i]

main = do
    print(result)
