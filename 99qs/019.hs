rotate :: [a] -> Int -> [a]

rotate l@(x:xs) n
    | n > 0 = rotate (xs ++ [x]) (n-1)
    | n == 0 = l
    | otherwise = rotate l (length l + n)

main = do
    print(rotate ['a','b','c','d','e','f','g','h'] 3)
    print(rotate ['a','b','c','d','e','f','g','h'] (-2))
