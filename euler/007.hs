nextPrime :: [Int] -> Int

nextPrime primes = nextPrimeHelper primes (last primes)
    where nextPrimeHelper primes n = if all (\x -> n `mod` x /= 0) primes then n else nextPrimeHelper primes (n + 1)

nthPrime n = nthPrimeHelper [2] (n-1)
    where nthPrimeHelper primes 0 = last primes
          nthPrimeHelper primes n = nthPrimeHelper (primes ++ [nextPrime primes]) (n - 1)

main = print(nthPrime 10001)
