import Data.List (group)

data Elem a = Single a | Multiple Int a deriving (Show)

decodeModified :: [Elem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = f x ++ decodeModified xs
  where
    f :: Elem a -> [a]
    f (Single x) = [x]
    f (Multiple n x) = replicate n x

main = print $ decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',
                               Multiple 2 'a',Single 'd',Multiple 4 'e']
