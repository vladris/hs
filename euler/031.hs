makeSum 0 _ = [[]]
makeSum n last = [[x] ++ y | x <- [200, 100, 50, 20, 10, 5, 2, 1], x <= last, n - x >= 0, y <- makeSum (n - x) x]

result = length (makeSum 200 201)

main = do
    print(result)
