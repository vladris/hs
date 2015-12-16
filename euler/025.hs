fib a b = a : fib b (a + b)

main = do
    print(1 + length (takeWhile (< 10^999) (fib 1 1)))
