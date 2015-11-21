import System.Random

removeAt :: [a] -> Int -> [a]
removeAt (x:xs) 0 = xs
removeAt (x:xs) i = [x] ++ (removeAt xs (i - 1))

rnd_select :: [a] -> Int -> IO [a]
rnd_select l 0 = return l
rnd_select l i = do
        t <- getStdRandom (randomR (0, length l - 1))
        rnd_select (removeAt l t) (i - 1)

main = do
    result <- rnd_select "abdefgh" 3
    print(result)
