length' :: Num b => [a] -> b
length' = foldl (\acc x -> acc + 1) 0 

length'' :: Num b => [a] -> b
length'' [] = 0
length'' (x:xs) = 1 + length'' xs
