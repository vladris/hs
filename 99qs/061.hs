data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r
countLeaves Empty = 0

leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r
leaves Empty = []

main = do
    print(countLeaves tree4)
    print(leaves tree4)
