module Problems31thru41 where

import Data.List (sort, group, find)
import Data.Maybe (fromJust)
import Control.Exception.Base (assert)

-- Problem 31
-- Determine whether a given integer number is prime.

isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | n == 2    = True
    | n == 3    = True
    | otherwise = all (0 /=) $ map (mod n) [2..(floor . sqrt . fromIntegral) n]

-- Problem 32
-- Determine the greatest common divisor of two positive integer numbers. Use
-- Euclid's algorithm.

myGCD :: Int -> Int -> Int
myGCD a b = head $ euclidean $ reverse $ sort $ map abs [a,b]
  where
    euclidean :: [Int] -> [Int]
    euclidean [a,0] = [a]
    euclidean [a,b] = euclidean [b, a `mod` b]

-- Problem 33
-- Determine whether two positive integer numbers are coprime. Two numbers
-- are coprime if their greatest common divisor equals 1.

coprime :: Int -> Int -> Bool
coprime a b = myGCD a b == 1

-- Problem 34
-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of
-- positive integers r (1 <= r < m) that are coprime to m.

totient :: Int -> Int
totient 1 = 1
totient n = length . filter (coprime n) $ [1..n-1]

-- Problem 35
-- Determine the prime factors of a given positive integer. Construct a flat
-- list containing the prime factors in ascending order.

primeFactors :: Int -> [Int]
primeFactors n
    | n <= 0    = undefined
    | n == 1    = []
    | otherwise = p : primeFactors (n `div` p)
  where
    p = smPrimeFacGT 1 n
    smPrimeFacGT :: Int -> Int -> Int
    smPrimeFacGT i
       | n `mod` (i+1) == 0 && isPrime (i+1) = const $ i+1
       | otherwise                           = smPrimeFacGT $ i+1

-- Problem 36
-- Determine the prime factors of a given positive integer. Construct a list
-- containing the prime factors and their multiplicity.

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = map (\x -> (head x, length x)) $ group $ primeFactors n

-- Problem 37
-- Calculate Euler's totient function phi(m) (improved).
-- See problem 34 for the definition of Euler's totient function. If the list
-- of the prime factors of a number m is known in the form of problem 36 then
-- the function phi(m) can be efficiently calculated as follows: Let ((p1 m1)
-- (p2 m2) (p3 m3) ...) be the list of prime factors (and their
-- multiplicities) of a given number m. Then phi(m) can be calculated with
-- the following formula:
-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
--          (p2 - 1) * p2 ** (m2 - 1) * 
--          (p3 - 1) * p3 ** (m3 - 1) * ...
-- Note that a ** b stands for the b'th power of a. 

phi :: Int -> Int
phi = product . map (\(p,m) -> (p-1) * p ^ (m-1)) . prime_factors_mult

-- Problem 39
-- A list of prime numbers.
-- Given a range of integers by its lower and upper limit, construct a list
-- of all prime numbers in that range.

primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]

-- Problem 40
-- Goldbach's conjecture.
-- Goldbach's conjecture says that every positive even number greater than 2
-- is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the
-- most famous facts in number theory that has not been proved to be correct
-- in the general case. It has been numerically confirmed up to very large
-- numbers (much larger than we can go with our Prolog system). Write a
-- predicate to find the two prime numbers that sum up to a given even integer.

goldbach :: Int -> (Int, Int)
goldbach n = fromJust $ find (\(x,y) -> isPrime x && isPrime y) candidates
  where
    candidates = map (\x -> (x, n'-x)) [1..n' `div` 2]
    n' = assert (even n && n > 2) n

-- Problem 41
-- Given a range of integers by its lower and upper limit, print a list of
-- all even numbers and their Goldbach composition.
-- In most cases, if an even number is written as the sum of two prime
-- numbers, one of them is very small. Very rarely, the primes are both
-- bigger than say 50. Try to find out how many such cases there are in the
-- range 2..3000.

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach [x | x <- [a..b], even x, x > 2]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b c = filter (\(x,y) -> x > c && y > c) $ goldbachList a b
