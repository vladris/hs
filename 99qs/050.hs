import Data.List -- for sort, insert

-- Node structure with custom ordering (based on second argument) 
data Node = Node String Int deriving Eq

instance Ord Node where compare (Node _ a) (Node _ b) = compare a b

-- prepend bit to second element of each pair if first element is in str
addBit l str bit = [(a, if elem a str then [bit] ++ b else b) | (a, b) <- l]

-- sort input on frequency and convert first element from char to string,
-- initialize bits with empty strings for each char
huffman l = huffmanCode (sort [(Node [a] b) | (a, b) <- l]) [(fst x, "") | x <- l]
    where
        -- Huffman code: pop first and second elements, min insert their sum, 
        -- and add '0' and '1' bits for them to the bits list
        huffmanCode [_] bits = bits
        huffmanCode ((Node x1 x2):(Node y1 y2):xs) bits =
            huffmanCode (insert (Node (x1 ++ y1) (x2 + y2)) xs) 
                        (addBit (addBit bits x1 '0') y1 '1')

main = do
   print(huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)])
