not' True = False
not' False = True

and' a b
    | a == True, b == True = True
    | otherwise = False

or' a b
    | a == False, b == False = False
    | otherwise = True

nand' a b = not' (and' a b)

nor' a b = not' (and' a b)

xor' a b
    | a /= b = True
    | otherwise = False

impl' a b
    | a == True, b == False = False
    | otherwise = True

equ' a b
    | a == b = True
    | otherwise = False

infixl 1 `not'`
infixl 3 `and'`
infixl 4 `or'`
infixl 3 `nand'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `impl'`
infixl 2 `equ'`

table2 :: (Bool -> Bool -> Bool) -> IO ()
table2 f = do
    print(True, True, f True True)
    print(True, False, f True False)
    print(False, True, f False True)
    print(False, False, f False False)


main = do
    table2 (\a b -> a `and'` (a `or'` not b))
