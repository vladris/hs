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

primes = getPrimes [] [2..10^6]

removeL 0 _ = True
removeL prime primes = elem prime primes && removeL (prime `mod` 10 ^ ((numLen prime) - 1)) primes

removeR 0 _ = True
removeR prime primes = elem prime primes && removeR (prime `div` 10) primes

result = [x | x <- primes, removeL x primes, removeR x primes]

main = do
    print(result)
    print(sum result - sum [2, 3, 5, 7])
