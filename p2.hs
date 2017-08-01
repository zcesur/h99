penultimate :: [a] -> a
penultimate xs
    | n < 2     = undefined
    | otherwise = xs !! (n-2)
    where n = length xs
