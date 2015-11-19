insertAt :: a -> [a] -> Int -> [a]

insertAt k l 1 = k : l
insertAt k (x:xs) n = x : insertAt k xs (n - 1)

main = do
    print(insertAt 'X' "abcd" 2)
