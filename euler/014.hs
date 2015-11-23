import Data.List

collatzSeq 1 = 1
collatzSeq n 
    | even n = 1 + collatzSeq (n `div` 2)
    | otherwise = 1 + collatzSeq (3 * n + 1)

result = fst (foldl (\acc x -> if snd acc > snd x then acc else x) (0, 0) [(n, collatzSeq n) | n <- [1..999999]])

main = do
    print(result)
