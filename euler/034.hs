unpack 0 = []
unpack n = (n `mod` 10):unpack (n `div` 10)

fact 0 = 1
fact n = n * fact (n - 1)

result = sum [x | x <- [3..10^6], sum (map (fact) (unpack x)) == x]

main = do
    print(result)
