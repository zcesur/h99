import Data.List (group)
import Test.HUnit

-- Problem 1
-- Find the last element of a list

last' :: [a] -> a
last' [] = undefined
last' [x] = x
last' (x:xs) = last' xs

last'' :: [a] -> a
last'' xs
    | null xs   = undefined
    | otherwise = xs !! (n-1)
    where n = length xs

-- Problem 2
-- Find the last but one element of a list. 

penultimate :: [a] -> a
penultimate xs
    | n < 2     = undefined
    | otherwise = xs !! (n-2)
    where n = length xs

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1. 

elementAt :: [a] -> Int -> a
elementAt xs k
    | n < k     = undefined
    | otherwise = xs !! (k-1)
    where n = length xs

-- Problem 4
-- Find the number of elements of a list. 

length' :: Num b => [a] -> b
length' = foldl (\acc x -> acc + 1) 0 

length'' :: Num b => [a] -> b
length'' [] = 0
length'' (x:xs) = 1 + length'' xs

-- Problem 5
-- Reverse a list. 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

reverse'' :: [a] -> [a]
reverse'' = foldr (\x acc -> acc++[x]) []

reverse''' :: [a] -> [a]
reverse''' = foldl (flip (:)) []

-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x). 

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively). 

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

-- Problem 8
-- Eliminate consecutive duplicates of list elements. 
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed. 

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == head xs = compress xs
    | otherwise    = x:compress xs

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

pack :: Eq a => [a] -> [[a]]
pack = foldr collect [[]]
    where collect :: (Eq a) => a -> [[a]] -> [[a]]
          collect x (a:acc)
            | null a      = [x]:acc
            | x == head a = (x:a):acc
            | otherwise   = [x]:a:acc

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E. 

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group

-- Some unit-tests

test1 = TestCase (assertEqual "Problem 1" (last' [1,2,3,4]) 4)
test2 = TestCase (assertEqual "Problem 2" (penultimate ['a'..'z']) 'y')
test3 = TestCase (assertEqual "Problem 3" (elementAt "haskell" 5) 'e')
test4 = TestCase (assertEqual "Problem 4" (length' "Hello, world!") 13)
test5 = TestCase (assertEqual "Problem 5" (reverse' [1,2,3,4]) [4,3,2,1])
test6 = TestCase (assertEqual "Problem 6" (isPalindrome "madamimadam") True)
test7 = TestCase (assertEqual "Problem 7" (flatten (List [ Elem 1
                                                         , List [ Elem 2
                                                                , List [ Elem 3
                                                                       , Elem 4 ]
                                                                , Elem 5 ]])) [1,2,3,4,5])
test8 = TestCase (assertEqual "Problem 8" (compress "aaaabccaadeeee") "abcade")
test9 = TestCase (assertEqual "Problem 9" (pack [ 'a', 'a', 'a', 'a'
                                                , 'b'
                                                , 'c', 'c'
                                                , 'a', 'a'
                                                , 'd'
                                                , 'e', 'e', 'e', 'e' ]) [ "aaaa"
                                                                        , "b"
                                                                        , "cc"
                                                                        , "aa"
                                                                        , "d"
                                                                        ,"eeee"])

test10 = TestCase (assertEqual "Problem 10" (encode "aaaabccaadeeee") [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')])

main = do
    let tests = TestList
            [ test1
            , test2
            , test3
            , test4
            , test5
            , test6
            , test7
            , test8
            , test9
            , test10 ]

    runTestTT tests
