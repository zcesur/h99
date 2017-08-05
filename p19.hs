rotate :: [a] -> Int -> [a]
rotate xs n = let m = n `mod` length xs in drop m xs ++ take m xs

main = do
    print $ rotate ['a','b','c','d','e','f','g','h'] 3
    print $ rotate ['a','b','c','d','e','f','g','h'] (-2)
