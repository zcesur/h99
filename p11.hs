import Data.List (group)

data Elem a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [Elem a]
encodeModified = map f . group
    where f :: [a] -> Elem a
          f x
            | n == 1    = Single (head x)
            | otherwise = Multiple n (head x)
            where n = length x

main = print $ encodeModified "aaaabccaadeeee"
