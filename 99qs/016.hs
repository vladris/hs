dropEvery :: [a] -> Int -> [a]

dropEvery a n = dropEveryHelper a n 1
    where
        dropEveryHelper [] _ _ = []
        dropEveryHelper (x:xs) n i = if i == n then dropEveryHelper xs n 1 else x : dropEveryHelper xs n (i + 1)

main = do
    print(dropEvery "abcdefghik" 3)
