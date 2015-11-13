data Elem a = Single a | Multiple Int a

decodeModified :: [Elem a] -> [a]

decodeModified [] = []
decodeModified (Single x:xs) = x : decodeModified xs
decodeModified (Multiple n x:xs) = replicate n x ++ decodeModified xs

main = do
    print(decodeModified [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'])
