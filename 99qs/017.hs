split :: [a] -> Int -> ([a], [a])

split a n = splitHelper ([], a) n
    where 
        splitHelper a 0 = a
        splitHelper (a, x:xs) n = splitHelper (a ++ [x], xs) (n - 1)

main = do
    print(split "abcdefghik" 3)
