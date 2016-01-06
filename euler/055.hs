isPalindrome n = let s = show n in s == reverse s

iterations = 50

isLychrel n 0 = True
isLychrel n i
    | isPalindrome n && i < iterations = False
    | otherwise = isLychrel (n + (read . reverse . show) n) (i - 1)

result = length [n | n <- [1..10000], isLychrel n iterations]

main = do
    print(result)
