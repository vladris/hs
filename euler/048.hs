result = sum [i^i | i <- [1..1000]] `mod` 10^10

main = do
    print(result)
