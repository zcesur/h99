import Data.List (nub, transpose, (\\))

combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations n xxs@(x:xs)
    | nub xxs /= xxs  = error "The list does not contain distinct elements."
    | n <= 0          = [[]]
    | n > length xxs  = [[]]
    | n == length xxs = [xxs]
    | otherwise       = map (x:) (combinations (n-1) xs) ++ combinations n xs


type People = [Char]
type Team = [People]

xs = "abcdefghi"

g :: Int -> [Team] -> [Team]
g _ [] = []
g n ((p:ps):ts) = map (++ps) (transpose [map (p \\) pts, pts]) ++ g n ts
  where
    pts = combinations n p

group3 :: People -> [Team]
group3 ps = g 4 $ g 3 $ g 2 $ [[ps]]
