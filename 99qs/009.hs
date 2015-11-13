makeNested :: [a] -> [[a]]

makeNested [x] = [[x]]
makeNested (x:xs) = [x] : makeNested xs

gather :: Eq a => [[a]] -> [[a]]

gather [x] = [x]
gather (x:y:xs) = if x !! 0 == y !! 0 then gather ((x ++ y) : xs) else x : gather (y : xs)

pack :: Eq a => [a] -> [[a]]

pack x = gather (makeNested x)

main = do
    print(pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'])
