module Problems11thru20 where

import Data.List (group)

-- Problem 11
-- Modified run-length encoding. 
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists. 

data Elem a = Single a | Multiple Int a deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [Elem a]
encodeModified = map f . group
    where f :: [a] -> Elem a
          f x
            | n == 1    = Single (head x)
            | otherwise = Multiple n (head x)
            where n = length x

-- Problem 12
-- Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11.
-- construct its uncompressed version.

decodeModified :: [Elem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = f x ++ decodeModified xs
  where
    f :: Elem a -> [a]
    f (Single x) = [x]
    f (Multiple n x) = replicate n x

-- Problem 13
-- Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method
-- directly. I.e. don't explicitly create the sublists containing the
-- duplicates, as in problem 9, but only count them. As in problem P11,
-- simplify the result list by replacing the singleton lists (1 X) by X.

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

-- Problem 14
-- Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

-- Problem 15
-- Replicate the elements of a list a given number of times. 

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = replicate n x ++ repli xs n 

repli' :: [a] -> Int -> [a]
repli' xs n = concat $ zipWith (replicate $) (replicate n n) xs

-- Problem 16
-- Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst $ filter (\x -> snd x /= n) $ zip xs $ cycle [1..n]

-- Problem 17
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.

split :: [a] -> Int -> ([a], [a])
split xs n = f ([], xs) n
  where
    f :: ([a], [a]) -> Int -> ([a], [a])
    f x 0 = x
    f (xs, []) n = (xs, [])
    f (xs, y:ys) n = f (xs++[y], ys) (n-1)

-- Problem 18
-- Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits
-- included). Start counting the elements with 1.

slice :: [a] -> Int -> Int -> [a]
slice xs m n = drop (m-1) $ take n xs

-- Problem 19
-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).

rotate :: [a] -> Int -> [a]
rotate xs n = let m = n `mod` length xs in drop m xs ++ take m xs

-- Problem 20
-- Remove the K'th element from a list.

removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (z, ys ++ zs)
  where
    ys = take (n-1) xs
    (z:zs) = drop (n-1) xs
