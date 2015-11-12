compress :: Eq a => [a] -> [a]

compress [x] = [x]
compress (x:y:xs) = if x == y then compress ([x] ++ xs) else [x] ++ compress ([y] ++ xs)

main = do
    print(compress "aaaabccaadeeee")
