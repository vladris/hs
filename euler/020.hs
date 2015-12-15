fact 1 = 1
fact n = n * fact (n - 1)

sumDigits 0 = 0
sumDigits n = n `mod` 10 + sumDigits (n `div` 10)

main = do
    print(sumDigits (fact 100))
