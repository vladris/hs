quicksortP :: [a] -> (a -> a -> Bool) -> [a]
quicksortP [] _ = []
quicksortP (x:xs) p = quicksortP [a | a <- xs, p a x] p ++ [x] ++ quicksortP [a | a <- xs, not (p a x)] p

lsort l = quicksortP l (\a b -> length a < length b)
lfsort l = quicksortP l (\a b -> length [as | as <- l, length as == length a] < length [bs | bs <- l, length bs == length b])

main = do
    print(lsort ["abc","de","fgh","de","ijkl","mn","o"])
    print(lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"])

