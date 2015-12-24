import Data.List

getNum num n = foldl (++) "" ((map (show) . map (*num)) [1..n])

result = maximum [read (getNum x y) :: Int | x <- [1..9999], y <- [2..9], sort (getNum x y) == "123456789"]

main = do
    print(result)
