removeAt :: Int -> [a] -> (a, [a])

removeAt n l = removeAtHelper n [] l
    where removeAtHelper 1 h (x:xs) = (x, h ++ xs)
          removeAtHelper n h (x:xs) = removeAtHelper (n-1) (h ++ [x]) xs

main = do
    print(removeAt 2 "abcd")
