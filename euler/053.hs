choose n 0 = 1
choose 0 k = 0
choose n k = choose (n - 1) (k - 1) * n `div` k

result = length [1 | n <- [1..100], r <- [1..n], choose n r > 10^6]

main = do
    print(result)
