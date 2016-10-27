import Data.Char
import Data.List

digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
fullWords n = intercalate "-" $ map (\d -> digits !! digitToInt d) (show n)

main = do
    print(fullWords 175)
