makePrimes n = [x | x <- [2..n], length [y | y <- [2..x - 1], x `mod` y == 0] == 0]

goldbach n primes = head [(x, y) | x <- primes, y <- primes, x + y == n]

goldbachList from to = let primes = makePrimes to in [goldbach n primes | n <- [from..to], n `mod` 2 == 0]

goldbachList' from to max = filter (\(a,b) -> a > max) (goldbachList from to)

main = do
    print(goldbachList 9 20)
    print(goldbachList' 4 2000 50)


