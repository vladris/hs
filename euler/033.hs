simplify a b
    | a `mod` 10 == b `div` 10 = (a `div` 10, b `mod` 10)
    | a `div` 10 == b `mod` 10 = (a `mod` 10, b `div` 10)
    | otherwise = (1, 0)

fractions = [simplify a b | a <- [10..99], b <- [a + 1..99], let (a', b') = simplify a b in a' * b == a * b']

folded = foldl (\(acc1, acc2) (x1, x2) -> (acc1 * x1, acc2 * x2)) (1, 1) fractions

result = last [(snd folded) `div` x | x <- [1..fst folded], (fst folded) `mod` x == 0, (snd folded) `mod` x == 0]

main = do
    print(result)
