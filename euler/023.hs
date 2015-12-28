limit = 28123

abundants = [x | x <- [1..limit], sum [y | y <- [1..x - 1], x `mod` y == 0] > x]

isSum n = not (null [x | x <- takeWhile (<=n) abundants, elem (n - x) (takeWhile (<=n - x) abundants)])
sums = [x + y | x <- abundants, y <- abundants, y <= x, x + y < limit]

result = sum [i | i <- [1..limit], not (isSum i)]

main = do
    print(result)
