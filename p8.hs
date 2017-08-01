compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == head xs = compress xs
    | otherwise    = x:compress xs

main = print $ compress "aaaabccaadeeee"
