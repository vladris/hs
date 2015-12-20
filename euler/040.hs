import Data.Char

digit d n
    | d <= length n = digitToInt (n !! (d - 1))
    | otherwise = digit (d - length n) (show ((read n) + 1))

result = product [digit (10 ^ x) "1" | x <- [0..6]]

main = do
    print(result)

