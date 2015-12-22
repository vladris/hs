import Data.List

divs = [(6, 2), (5, 3), (4, 5), (3, 7), (2, 11), (1, 13), (0, 17)]
ndiv n = foldl (&&) True [((n `div` 10^(fst x)) `mod` 1000) `mod` (snd x) == 0 | x <- divs]

makeNum [] = 0
makeNum (x:xs) = x + 10 * makeNum xs

result = sum [makeNum n | n <- permutations [0..9], let num = makeNum n in ndiv num]

main = do
    print(result)
