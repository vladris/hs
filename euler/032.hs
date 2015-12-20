import Data.List

makeNum [] = 0
makeNum (x:xs) = x + makeNum xs * 10

getNums n x y = (makeNum (take x n), makeNum (take (y - x) (drop x n)), makeNum (drop y n))

result = sum (nub [makeNum (drop y n) | n <- permutations [1..9], x <- [1..4], y <- [x..5], let (a, b, c) = getNums n x y in a * b == c])

main = do
    print(result)
