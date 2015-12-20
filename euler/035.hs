getPrimes primes [] = primes
getPrimes primes (x:xs) = if isPrime x primes
                          then getPrimes (primes ++ [x]) xs
                          else getPrimes primes xs
    where isPrime n [] = True
          isPrime n (x:xs) = if x^2 > n
                             then True
                             else (n `mod` x /= 0) && isPrime n xs

numLen 0 = 0
numLen n = 1 + numLen (n `div` 10)

rotate n = (n `mod` 10) * 10 ^ ((numLen n) - 1) + (n `div` 10)

isCircular p primes = isCircularHelper p primes ((numLen p) - 1)
    where isCircularHelper p primes 0 = True
          isCircularHelper p primes n = let k = rotate p in if not (elem k primes) then False else isCircularHelper k primes (n - 1)

primes = getPrimes [] [2..10^6]

result = length [p | p <- primes, isCircular p primes]

main = do
    print(result)
