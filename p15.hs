repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n 

repli' :: [a] -> Int -> [a]
repli' xs n = concat $ zipWith (replicate $) (replicate n n) xs

main = print $ repli "abc" 3
