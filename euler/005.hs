divisible :: Int -> Int -> Bool

divisible x 2 = x `mod` 2 == 0
divisible x d = x `mod` d == 0 && divisible x (d - 1)

result = head [x | x <- [1..], divisible x 20]

main = print result
