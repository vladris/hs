myGCD :: Int -> Int -> Int
myGCD x y
    | y == 0 = x
    | x < 0 = myGCD (-x) y
    | y < 0 = myGCD x (-y)
    | y > x = myGCD y x
    | otherwise = myGCD (x - y) y

main = do
    print([myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6])
