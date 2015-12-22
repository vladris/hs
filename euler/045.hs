triangle n = n * (n + 1) `div` 2
pentagonal = [n * (3 * n - 1) `div` 2 | n <- [1..]]
hexagonal = [n * (2 * n - 1) | n <- [1..]]

isElem x elems = let taken = takeWhile (<=x) elems in if null taken then False else last taken == x

result = head [triangle x | x <- [286..], isElem (triangle x) pentagonal, isElem (triangle x) hexagonal]

main = do
    print(result)
