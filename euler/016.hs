sumDigits 0 = 0
sumDigits n = n `mod` 10 + sumDigits (n `div` 10)

result = sumDigits (2 ^ 1000)

main = do
    print(result)
