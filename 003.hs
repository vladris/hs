prime_factors :: Int -> [Int]

prime_factors 1 = []
prime_factors n
  | factors == [] = [n]
  | otherwise = factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. floor(sqrt(fromIntegral(n - 1)))]

result = last (prime_factors 600851475143)

main = print result
