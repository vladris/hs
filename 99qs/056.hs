data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = (mirror l1 r2) && (mirror r1 l2)
mirror _ _ = False

main = do
    print(symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty))
    print(symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)))

