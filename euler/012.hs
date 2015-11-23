import Data.List

countDivisors n = 2 * length (filter (\x -> n `mod` x == 0) [1..floor(sqrt(fromIntegral n))])

result = find (\(x, n) -> n > 500) [let n = (x * (x + 1)) `div` 2 in (n, countDivisors n) :: (Int, Int) | x <- [1..]]

main = do
    print(result)
