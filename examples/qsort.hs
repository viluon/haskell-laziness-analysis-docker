qsort [] = []
qsort trace@(a:as) = qsort left ++ [a] ++ qsort right
  where (left, right) = (filter (<=a) as, filter (>a) as)

main = print $ qsort [1 + 8, 4, 0, 3, 1, 23, 11, 18]

