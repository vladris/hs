data Elem a = Single a | Multiple Int a deriving (Show)

pack :: Eq a => [a] -> [[a]]

pack x = gather (map (\x -> [x]) x)
    where
        gather [x] = [x]
        gather (x:y:xs) = if x !! 0 == y !! 0 
            then gather ((x ++ y) : xs) 
            else x : gather (y : xs)

encode :: Eq a => [a] -> [Elem a]

encode x = map (\x -> if length x == 1 
                        then Single (x !! 0)
                        else Multiple (length x) (x !! 0)) 
               (pack x) 

main = do
    print(encode ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])
