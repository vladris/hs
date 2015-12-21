import Data.List

isPrime :: Int -> Bool
isPrime n = null [d | d <- [2..round(sqrt (fromIntegral n))], n `mod` d == 0]

makeNum :: [Int] -> Int
makeNum [] = 0
makeNum (x:xs) = x + (makeNum xs) * 10

numbers n = let nums = [num | num <- map makeNum (permutations [1..n]), isPrime num] in if null nums then 0 else maximum nums

main = do
    print(maximum [numbers n | n <- [1..9]])
