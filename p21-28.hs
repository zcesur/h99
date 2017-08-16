import Data.List (nub, (\\), transpose)
import System.Random
import Test.HUnit

-- Problem 21
-- Insert an element at a given position into a list.

insertAt :: a -> [a] -> Int -> [a]
insertAt x ys n = concat [fst chunks, [x], snd chunks]
  where
    chunks = splitAt (n-1) ys

test1 = TestCase $ assertEqual "Problem 21" "aXbcd" $ insertAt 'X' "abcd" 2

-- Problem 22
-- Create a list containing all integers within a given range.

range :: Int -> Int -> [Int]
range m n = [m..n]

test2 = TestCase $ assertEqual "Problem 22" [4,5,6,7,8,9] $ range 4 9

-- Problem 23
-- Extract a given number of randomly selected elements from a list.

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    let indices = take n (randomRs (0, (length xs) - 1) gen)
    return $ map (xs !!) indices

-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.

diff_select :: Int -> Int -> IO [Int]
diff_select n m
    | n < 0     = undefined
    | m < 0     = return []
    | n > m     = error "Not enough elements in the set"
    | otherwise = do
    gen <- getStdGen
    let draws = randomRs (1, m) gen
    return $ last . takeUntil (\x -> length x == n) . scanl addIfDistinct [] $ draws
  where
    takeUntil _ []                 = []
    takeUntil p (x:xs) | p x       = [x]
                       | otherwise = x : takeUntil p xs
    
    addIfDistinct acc x
        | x `elem` acc = acc
        | otherwise    = x:acc

-- Problem 25
-- Generate a random permutation of the elements of a list.

rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
    gen <- getStdGen
    fmap (map (xs !!)) idx
  where
    idx = fmap (map (subtract 1)) $ diff_select n n
    n = length xs

-- Problem 26
-- Generate the combinations of K distinct objects chosen from the N elements of a list
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.

combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations n xxs@(x:xs)
    | nub xxs /= xxs  = error "The list does not contain distinct elements."
    | n <= 0          = [[]]
    | n > length xxs  = [[]]
    | n == length xxs = [xxs]
    | otherwise       = map (x:) (combinations (n-1) xs) ++ combinations n xs

test6 = TestCase $ assertEqual "Problem 26" 220 $ length $ combinations 3 [1..12]

--Problem 27a
--Group the elements of a set into disjoint subsets.
--In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list. 

type People = [Char]
type Team = [People]

rearrange :: Int -> [Team] -> [Team]
rearrange _ [] = []
rearrange n ((p:ps):ts)
    | n > length p  = error "Not enough members."
    | n == length p = [(p:ps)] ++ rearrange n ts
    | otherwise     = map (++ps) (splitUp n p) ++ rearrange n ts
  where
    splitUp :: Int -> People -> [Team]
    splitUp n xs = transpose [map (xs \\) (combinations n xs), combinations n xs]

group3 :: People -> [Team]
group3 xs = rearrange 4 $ rearrange 3 $ rearrange 2 $ [[xs]]

test7a = TestCase $ assertEqual "Problem 27a" 1260 $ length $ group3 ['a'..'i']

--Problem 27b
--Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.

group :: [Int] -> People -> [Team]
group ns xs = groupHelper ns [[xs]]
  where
    groupHelper :: [Int] -> [Team] -> [Team]
    groupHelper [] xs = xs
    groupHelper (n:ns) xs = groupHelper ns $ rearrange n $ xs

test7b1 = TestCase $ assertEqual "Problem 27b" 1260 $ length $ group [2,3,4] ['a'..'i']
test7b2 = TestCase $ assertEqual "Problem 27b" 756 $ length $ group [2,2,5] ['a'..'i']

--Problem 28a
--Sorting a list of lists according to length of sublists
--We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = lsort smaller ++ [x] ++ lsort larger
  where
    smaller = [e | e <- xs, length e < length x]
    larger = [e | e <- xs, length e >= length x]

test8a = TestCase $ assertEqual "Problem 28a" expected actual
  where
    expected = ["o","de","de","mn","abc","fgh","ijkl"]
    actual = lsort ["abc","de","fgh","de","ijkl","mn","o"]

--Problem 28b
--Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

splitByLen :: [[a]] -> [[[a]]]
splitByLen xs = foldl f [] xs
  where
    f [] x = [[x]]
    f (a:as) x
        | length (head a) == length x = (x:a):as
        | otherwise                   = [x]:(a:as)

lfsort :: [[a]] -> [[a]]
lfsort xs = concat $ lsort $ splitByLen $ lsort xs

test8b = TestCase $ assertEqual "Problem 28b" expected actual
  where
    expected = ["ijkl","o","fgh","abc","mn","de","de"]
    actual = lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]

main = do
    let tests = TestList
            [ test1
            , test2 
            , test6 
            , test7a
            , test7b1
            , test7b2
            , test8a
            , test8b ]

    runTestTT tests 
