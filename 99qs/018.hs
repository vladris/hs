slice :: [a] -> Int -> Int -> [a]

slice [] _ _ = []
slice (x:xs) i j = if j == 0
                   then []
                   else if i == 1
                        then x : slice xs 1 (j - 1)
                        else slice xs (i - 1) (j - 1)

main = do
    print(slice ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'k'] 3 7)
