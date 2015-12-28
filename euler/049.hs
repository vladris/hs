import Data.List

primes = [i | i <- [1000..9999], null [j | j <- [2..round(sqrt(fromIntegral i))], i `mod` j == 0]]

sameDigits n = [p | p <- primes, sort(show n) == sort(show p)]

findSeq numbers = [(j, i, i + (i - j)) | i <- numbers, j <- numbers, i > j, elem (i + (i - j)) numbers]

result = [(findSeq . sameDigits) n | n <- primes, n == (head . sameDigits) n, (not . null . findSeq . sameDigits) n]

main = do
    print(result)
