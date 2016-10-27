data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

internals (Branch _ Empty Empty) = []
internals (Branch x l r) = [x] ++ internals l ++ internals r
internals Empty = []

atLevel Empty _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch _ l r) n = atLevel l (n - 1) ++ atLevel r (n - 1)

main = do
    print(internals tree4)
    print(atLevel tree4 2)
