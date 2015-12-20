import Data.Bits

binary 0 = ""
binary n = ("01" !! (n .&. 1)) : binary (n `shiftR` 1)

result = sum [x | x <- [1..10^6], let strX = show x in strX == reverse strX, let bstrX = binary x in bstrX == reverse bstrX]

main = do
    print(result)
