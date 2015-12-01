primesR start end = [x | x <- [start..end], length [y | y <- [2..x - 1], x `mod` y == 0] == 0]

main = do
    print(primesR 10 20)
