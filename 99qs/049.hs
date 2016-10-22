import Data.Bits
import Text.Printf

grayBits :: Int -> Int
grayBits n = n `xor` (shift n (-1))

showBits :: Int -> Int -> String
showBits bits i = printf ("%0" ++ (show bits) ++ "b") i

gray n = map (showBits n . grayBits) [0..2^n - 1]

main = do
    print(gray 3)
