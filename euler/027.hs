import Data.List
import Data.Ord

isPrime n = if n < 2 then False else null [x | x <- [2..n - 1], n `mod` x == 0]

primes a b n
    | isPrime (n^2 + a * n + b) = 1 + primes a b (n + 1)
    | otherwise = 0

main = do
    print(let (_, a, b) = maximumBy (comparing (\(m, _, _) -> m)) [(primes a b 0, a, b) | a <- [-1000..1000], b <- [-1000..1000]] in a * b)
