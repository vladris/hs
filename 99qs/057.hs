data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- symmetric and mirror from 056
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = (mirror l1 r2) && (mirror r1 l2)
mirror _ _ = False
-- 

construct xs = foldl add Empty xs
    where
        add Empty x = Branch x Empty Empty
        add (Branch a l r) x = if x < a then Branch a (add l x) r else Branch a l (add r x)

main = do
    print(symmetric . construct $ [5, 3, 18, 1, 4, 12, 21])
    print(symmetric . construct $ [3, 2, 5, 7, 1])

