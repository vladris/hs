sumDigits 0 = 0
sumDigits n = (n `mod` 10) ^ 5 + sumDigits (n `div` 10)

-- 10^6 as 9^5 * 6 < 10^6
result = sum [x | x <- [10..10^6], sumDigits x == x]

main = do
    print(result)
