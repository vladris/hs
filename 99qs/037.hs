-- from 036
import Data.List

primeFactors n = primeFactorsHelper n [x | x <- [2..floor(sqrt(fromIntegral n))], length [y | y <- [2..x - 1], x `mod` y == 0] == 0]

primeFactorsHelper n [] = if n > 1 then [n] else []
primeFactorsHelper n l@(x:xs) = if n `mod` x == 0 then [x] ++ primeFactorsHelper (n `div` x) l else primeFactorsHelper n xs

prime_factors_mult n = map (\x -> (head x, length x)) (group (primeFactors n))
--

phi n = foldl (\acc x -> acc * ((fst x) - 1) * (fst x) ^ ((snd x) - 1)) 1 (prime_factors_mult n)

main = do
    print(phi 10)
