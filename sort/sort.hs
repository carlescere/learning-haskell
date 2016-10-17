quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [a] = [a]
quicksort (x:xs) = quicksort lower ++ x : quicksort greater
  where lower = [y | y <- xs, y <= x]
        greater = [y | y <- xs, y > x]

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [a] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)
