reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

reverse'' :: [a] -> [a]
reverse'' = foldr (\x acc -> acc++[x]) []

reverse''' :: [a] -> [a]
reverse''' = foldl (flip (:)) []
