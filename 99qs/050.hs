import Data.List -- for sortOn

-- prepend bit to second element of each pair if first element is in str
addBit l str bit = [(a, if elem a str then [bit] ++ b else b) | (a, b) <- l]

-- insert keeping sort order by second element
minInsert l (a, b) = [(x, y) | (x, y) <- l, y < b] ++ [(a, b)] ++ [(x, y) | (x, y) <- l, y > b]

-- Huffman code: pop first and second elements, min insert their sum, and add '0' and '1' bits for them to the bits list
huffmanCode [_] bits = bits
huffmanCode ((x1,x2):(y1,y2):xs) bits = huffmanCode (minInsert xs (x1 ++ y1, x2 + y2)) (addBit (addBit bits x1 '0') y1 '1')

-- sort input on frequency and convert first element from char to string, initialize bits with empty strings for each char
huffman l = huffmanCode (sortOn (snd) [([a], b) | (a, b) <- l]) [(a, "") | (a, _) <- l]

main = do
   print(huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)])
