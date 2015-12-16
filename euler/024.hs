import Data.List

permutation [] = [[]]
permutation xs = [x:ys | x <- xs, ys <- permutation (delete x xs)]

main = do
    print((permutation [0..9]) !! (10^6 - 1))
