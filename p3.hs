elementAt :: [a] -> Int -> a
elementAt xs k
    | n < k     = undefined
    | otherwise = xs !! (k-1)
    where n = length xs
