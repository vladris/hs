import Data.Char

digitSum n = sum (map (digitToInt) (show n))

result = maximum [digitSum (a^b) | a <- [1..100], b <- [1..100]]

main = do
    print(result)
