palindrome (x, _) = x == reverse x

result = maximum (map (snd) (filter (palindrome) [(show (x * y), x * y) | x <- [100..999], y <- [100..999]]))

main = print result
