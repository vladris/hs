repli :: [a] -> Int ->[a]

repli [] _ = []
repli (x:xs) n = [x | _ <- [1..n]] ++ repli xs n

main = do
    print(repli "abc" 3)
