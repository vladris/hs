import Data.List
import Data.Ord

triangles p = length [1 | a <- [1..p], b <- [a..p - a], a^2 + b^2 == (p - a - b)^2]

result = maximumBy (comparing snd) [(p, triangles p) | p <- [1..1000]]

main = do
    print(result)
