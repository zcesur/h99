pack :: Eq a => [a] -> [[a]]
pack = foldr collect [[]]
    where collect :: (Eq a) => a -> [[a]] -> [[a]]
          collect x (a:acc)
            | null a      = [x]:acc
            | x == head a = (x:a):acc
            | otherwise   = [x]:a:acc

main = print $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c',
                     'a', 'a', 'd', 'e', 'e', 'e', 'e']
