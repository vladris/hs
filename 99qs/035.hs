primeFactors n = primeFactorsHelper n [x | x <- [2..floor(sqrt(fromInteger n))], length [y | y <- [2..x - 1], x `mod` y == 0] == 0]

primeFactorsHelper n [] = if n > 1 then [n] else []
primeFactorsHelper n l@(x:xs) = if n `mod` x == 0 then [x] ++ primeFactorsHelper (n `div` x) l else primeFactorsHelper n xs

main = do
    print(primeFactors 315)
