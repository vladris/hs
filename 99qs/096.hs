import Data.Char

identifier [] = False
identifier (x:xs) = isLetter x && alnumdash xs
    where alnumdash [] = True
          alnumdash (x:xs)
              | x == '-' = xs /= [] && isAlphaNum (head xs) && alnumdash (tail xs)
              | isAlphaNum x = alnumdash xs
              | otherwise = False

main = do
    print(identifier "this-is-a-long-identifier")
    print(identifier "this-ends-in-")
    print(identifier "two--hyphens")
