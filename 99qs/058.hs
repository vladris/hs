data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- cbalTree from 055
cbalTree 0 = [Empty]
cbalTree n
    | m * 2 == n - 1 = [Branch 'x' a b | a <- cbalTree m, b <- cbalTree m]
    | otherwise = concat [[Branch 'x' a b, Branch 'x' b a] | a <- cbalTree m, b <- cbalTree (m + 1)]
    where m = (n - 1) `div` 2
--

symCbalTrees n
    | n `mod` 2 == 0 = []
    | otherwise = [Branch 'x' t (reverse t) | t <- cbalTree (n `div` 2)]
    where
        reverse Empty = Empty
        reverse (Branch a l r) = Branch a (reverse r) (reverse l)

main = do
    print(symCbalTrees 5)
