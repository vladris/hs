combinations 0 _ = [[]] 
combinations i l = foldl (++) [] [map (\xs -> (l !! t):xs) (combinations (i - 1) (take t l)) | t <- [0..(length l) - 1]]

main = do
    print(combinations 3 "abcdef")
