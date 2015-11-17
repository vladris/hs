data Elem a = Single a | Multiple Int a deriving (Show)

encodeDirect :: Eq a => [a] -> [Elem a]

encodeDirect a = encodeDirectImpl([Multiple 1 x | x <- a])

encodeDirectImpl :: Eq a => [Elem a] -> [Elem a]

encodeDirectImpl [] = []
encodeDirectImpl [Multiple n x] = [Multiple n x]
encodeDirectImpl (Multiple n x:Multiple _ y:xs) = if x == y
                                                    then encodeDirectImpl((Multiple (n + 1) x) : xs)
                                                    else if n == 1
                                                        then (Single x) : encodeDirectImpl((Multiple 1 y) : xs)
                                                        else (Multiple n x) : encodeDirectImpl((Multiple 1 y) : xs)

main = do
    print(encodeDirect "aaaabccaadeeee")
