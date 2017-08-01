last' :: [a] -> a
last' [] = undefined
last' [x] = x
last' (x:xs) = last' xs

last'' :: [a] -> a
last'' xs
    | null xs   = undefined
    | otherwise = xs !! (n-1)
    where n = length xs
