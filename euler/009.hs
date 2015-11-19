result = fromInteger (round (head [x * y * sqrtSqrSum x y | x <- [1..1000], y <- [1..1000], isInt (sqrtSqrSum x y), x + y + sqrtSqrSum x y == 1000]))
    where isInt x = x == fromInteger (round x)
          sqrtSqrSum x y = sqrt (x^2 + y^2)

main = do
    print(result)
