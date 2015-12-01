import Data.List

-- from 035
primeFactors n = primeFactorsHelper n [x | x <- [2..floor(sqrt(fromInteger n))], length [y | y <- [2..x - 1], x `mod` y == 0] == 0]

primeFactorsHelper n [] = if n > 1 then [n] else []
primeFactorsHelper n l@(x:xs) = if n `mod` x == 0 then [x] ++ primeFactorsHelper (n `div` x) l else primeFactorsHelper n xs
--

prime_factors_mult n = map (\x -> (head x, length x)) (group (primeFactors n))

main = do
    print(prime_factors_mult 315)
