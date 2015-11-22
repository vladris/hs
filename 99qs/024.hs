import System.Random

diff_select i m = diffSelectHelper [1..m] i []
    where diffSelectHelper l 0 r = return r
          diffSelectHelper l i r = do
                t <- getStdRandom (randomR (0, length l - 1))
                let(l', x) = (take t l ++ drop (t + 1) l, l !! t) in diffSelectHelper l' (i - 1) (x:r)

main = do
    result <- diff_select 6 49
    print(result)
