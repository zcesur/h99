dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst $ filter (\x -> snd x /= n) $ zip xs $ cycle [1..n]

main = print $ dropEvery "abcdefghik" 3
