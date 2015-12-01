primes n = [x | x <- [2..n], length [y | y <- [2..x - 1], x `mod` y == 0] == 0]

goldbach n = head [(x, y) | x <- primes n, y <- primes n, x + y == n]

main = do
    print(goldbach 28)
