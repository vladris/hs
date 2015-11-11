fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

result = sum [x | x <- (fst (span (< 4000000) fibs)), x `mod` 2 == 0]

main = print result
