split :: [a] -> Int -> ([a], [a])
split xs n = f ([], xs) n
  where
    f :: ([a], [a]) -> Int -> ([a], [a])
    f x 0 = x
    f (xs, []) n = (xs, [])
    f (xs, y:ys) n = f (xs++[y], ys) (n-1)

main = print $ split "abcdefghik" 3
