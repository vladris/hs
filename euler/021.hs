divisors n = [x | x <- [1..n - 1], n `mod` x == 0]

isAmicable n = let ds = sum (divisors n) in ds /= n && sum (divisors ds) == n

main = do
    print(sum (filter isAmicable [1..10000]))
