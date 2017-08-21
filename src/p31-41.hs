import Data.List (sort, group, find)
import Data.Maybe (fromJust)
import Control.Exception.Base (assert)
import Test.HUnit (assertEqual, runTestTT, Test(..))

-- Problem 31
-- Determine whether a given integer number is prime.

isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | n == 2    = True
    | n == 3    = True
    | otherwise = all (0 /=) $ map (mod n) [2..(floor . sqrt . fromIntegral) n]

test1a = TestCase $ assertEqual "Problem 31" expected actual
  where
    expected = True
    actual = and $ map isPrime [2,3,5,7,11,13,17,19]

test1b = TestCase $ assertEqual "Problem 31" expected actual
  where
    expected = False
    actual = or $ map isPrime [(-2),(-1),0,1,4,6,8,9]

-- Problem 32
-- Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.

myGCD :: Int -> Int -> Int
myGCD a b = head $ euclidean $ reverse $ sort $ map abs [a,b]
  where
    euclidean :: [Int] -> [Int]
    euclidean [a,0] = [a]
    euclidean [a,b] = euclidean [b, a `mod` b]

test2 = TestCase $ assertEqual "Problem 32" expected actual
  where
    expected = [9,3,3]
    actual = [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]

-- Problem 33
-- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

coprime :: Int -> Int -> Bool
coprime a b = myGCD a b == 1

test3 = TestCase $ assertEqual "Problem 33" True $ coprime 35 64

-- Problem 34
-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.

totient :: Int -> Int
totient 1 = 1
totient n = length . filter (coprime n) $ [1..n-1]

test4 = TestCase $ assertEqual "Problem 34" 4 $ totient 10

-- Problem 35
-- Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.

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

test5 = TestCase $ assertEqual "Problem 35" [3,3,5,7] $ primeFactors 315

-- Problem 36
-- Determine the prime factors of a given positive integer.

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult n = map (\x -> (head x, length x)) $ group $ primeFactors n

test6 = TestCase $ assertEqual "Problem 36" expected actual
  where
    expected = [(3,2),(5,1),(7,1)]
    actual = prime_factors_mult 315

-- Problem 37
-- Calculate Euler's totient function phi(m) (improved).
-- See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36 then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
-- phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
--          (p2 - 1) * p2 ** (m2 - 1) * 
--          (p3 - 1) * p3 ** (m3 - 1) * ...
-- Note that a ** b stands for the b'th power of a. 

phi :: Int -> Int
phi = product . map (\(p,m) -> (p-1) * p ^ (m-1)) . prime_factors_mult

test7 = TestCase $ assertEqual "Problem 37" 4 $ phi 10

-- Problem 39
-- A list of prime numbers.
-- Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.

primesR :: Int -> Int -> [Int]
primesR a b = filter isPrime [a..b]

test9 = TestCase $ assertEqual "Problem 39" [11,13,17,19] $ primesR 10 20

-- Problem 40
-- Goldbach's conjecture.
-- Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than we can go with our Prolog system). Write a predicate to find the two prime numbers that sum up to a given even integer.

goldbach :: Int -> (Int, Int)
goldbach n = fromJust $ find (\(x,y) -> isPrime x && isPrime y) candidates
  where
    candidates = map (\x -> (x, n'-x)) [1..n' `div` 2]
    n' = assert (even n && n > 2) n

test10 = TestCase $ assertEqual "Problem 40" (5,23) $ goldbach 28

-- Problem 41
-- Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
-- In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50. Try to find out how many such cases there are in the range 2..3000.

goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = map goldbach [x | x <- [a..b], even x, x > 2]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b c = filter (\(x,y) -> x > c && y > c) $ goldbachList a b

test11a = TestCase $ assertEqual "Problem 41" expected actual
  where
    expected = [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
    actual = goldbachList 9 20

test11b = TestCase $ assertEqual "Problem 41" expected actual
  where
    expected = [(73,919),(61,1321),(67,1789),(61,1867)]
    actual = goldbachList' 4 2000 50

main = do
    let tests = TestList
            [ test1a 
            , test1b
            , test2
            , test3
            , test4
            , test5
            , test6
            , test7
            , test9
            , test10
            , test11a
            , test11b ]

    runTestTT tests 
