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

g :: Int -> [Team] -> [Team]
g _ [] = []
g n ((p:ps):ts) = map (++ps) (transpose [map (p \\) pts, pts]) ++ g n ts
  where
    pts = combinations n p

group3 :: People -> [Team]
group3 ps = g 4 $ g 3 $ g 2 $ [[ps]]

test7a = TestCase $ assertEqual "Problem 27a" 1260 $ length $ group3 ['a'..'i']

main = do
    let tests = TestList
            [ test1
            , test2 
            , test6 
            , test7a ]

    runTestTT tests 
