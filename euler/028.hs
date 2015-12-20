spiral 1 = 1
spiral n = spiral (n - 2) + 4 * (n - 2) ^ 2 + 10 * (n - 1)

main = do
    print(spiral 1001)
