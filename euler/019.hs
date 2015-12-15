import Data.Time.Calendar

addDoW 7 = 1
addDoW n = n + 1

start = fromGregorian 1900 1 1

makeDays _ _ 0 = []
makeDays previousDay previousDoW n = let day = addDays 1 previousDay
                                         dow = addDoW previousDoW 
                                     in (day, dow) : makeDays day dow (n - 1)

dayFilter (day,dow)
    | let (y, _, _) = toGregorian day in y < 1901 || y > 2000 = False
    | dow == 7 && let (_, _, d) = toGregorian day in d == 1 = True
    | otherwise = False

main = do
    print(length (filter dayFilter (makeDays start 1 400000)))
