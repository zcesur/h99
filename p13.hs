data Elem a = Single a | Multiple Int a deriving (Show)

encodeDirect :: Eq a => [a] -> [Elem a]
encodeDirect = foldr f []
  where
    f :: Eq a => a -> [Elem a] -> [Elem a]
    f x [] = [Single x]
    f x (y:ys)
      | x == getElem y = (increment y):ys
      | otherwise      = (Single x):y:ys

getElem :: Elem a -> a
getElem (Single x) = x
getElem (Multiple _ x) = x

increment :: Elem a -> Elem a
increment (Single x) = Multiple 2 x
increment (Multiple n x) = Multiple (n+1) x

main = print $ encodeDirect "aaaabccaadeeee"
