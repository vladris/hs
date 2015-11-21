getPrimes primes total [] = total
getPrimes primes total (x:xs) = if isPrime x primes
                          then getPrimes (primes ++ [x]) (total + x) xs
                          else getPrimes primes total xs
    where isPrime n [] = True
          isPrime n (x:xs) = if x^2 > n
                             then True
                             else (n `mod` x /= 0) && isPrime n xs

result = getPrimes [] 0 [2..2000000]

main = print result
