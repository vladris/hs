import System.Random

rnd_permu l = rndPermuHelper l (length l) []
    where rndPermuHelper l 0 r = return r
          rndPermuHelper l i r = do
                t <- getStdRandom (randomR (0, length l - 1))
                let(l', x) = (take t l ++ drop (t + 1) l, l !! t) in rndPermuHelper l' (i - 1) (x:r)

main = do
    result <- rnd_permu "abcdef"
    print(result)
