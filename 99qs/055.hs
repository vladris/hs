data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

cbalTree 0 = [Empty]
cbalTree n
    | m * 2 == n - 1 = [Branch 'x' a b | a <- cbalTree m, b <- cbalTree m]
    | otherwise = concat [[Branch 'x' a b, Branch 'x' b a] | a <- cbalTree m, b <- cbalTree (m + 1)]
    where m = (n - 1) `div` 2

main = do
    print(cbalTree 4)

