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

table :: (Bool -> Bool -> Bool) -> IO ()
table f = do
    print(True, True, f True True)
    print(True, False, f True False)
    print(False, True, f False True)
    print(False, False, f False False)


main = do
    table (\a b -> (and' a (or' a b)))
