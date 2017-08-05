slice :: [a] -> Int -> Int -> [a]
slice xs m n = drop (m-1) $ take n xs

main = print $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
