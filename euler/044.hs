pentagonal n = n * (3 * n - 1) `div` 2

pent ps i = let found = [x | x <- [i, i - 1..1], let px = pentagonal x
                                                     py = (head ps) - px 
                                                 in elem py ps && elem (abs (px - py)) ps]
            in if null found then pent ((pentagonal (i + 1)):ps) (i + 1) else abs ((pentagonal . head) found - ((head ps) - (pentagonal . head) found))

main = do
    print(pent [1] 1)
