letters n
    | n ==    0 = 0
    | n ==    1 = length "one"
    | n ==    2 = length "two"
    | n ==    3 = length "three"
    | n ==    4 = length "four"
    | n ==    5 = length "five"
    | n ==    6 = length "six"
    | n ==    7 = length "seven"
    | n ==    8 = length "eight"
    | n ==    9 = length "nine"
    | n ==   10 = length "ten"
    | n ==   11 = length "eleven"
    | n ==   12 = length "twelve"
    | n ==   13 = length "thirteen"
    | n ==   14 = length "fourteen"
    | n ==   15 = length "fifteen"
    | n ==   16 = length "sixteen"
    | n ==   17 = length "seventeen"
    | n ==   18 = length "eighteen"
    | n ==   19 = length "nineteen"
    | n ==   20 = length "twenty"
    | n  <   30 = letters 20 + letters (n - 20)
    | n ==   30 = length "thirty"
    | n  <   40 = letters 30 + letters (n - 30)
    | n ==   40 = length "forty"
    | n  <   50 = letters 40 + letters (n - 40)
    | n ==   50 = length "fifty"
    | n  <   60 = letters 50 + letters (n - 50)
    | n ==   60 = length "sixty"
    | n  <   70 = letters 60 + letters (n - 60)
    | n ==   70 = length "seventy"
    | n  <   80 = letters 70 + letters (n - 70)
    | n ==   80 = length "eighty"
    | n  <   90 = letters 80 + letters (n - 80)
    | n ==   90 = length "ninety"
    | n  <  100 = letters 90 + letters (n - 90)
    | n == 1000 = length "one" + length "thousand"
    | otherwise = letters (n `div` 100) + length "hundred" + if n `mod` 100 /= 0 then length "and" + letters (n `mod` 100) else 0

result = sum [letters n | n <- [1..1000]]

main = do
    print(result)
