elementAt :: [a] -> Int -> a

elementAt (x:xs) 1  = x
elementAt (x:xs) n = elementAt xs (n-1)

main = do
    print(elementAt [1, 2, 3] 2)
    print(elementAt "haskell" 5)
