import Data.List (sort, group)
import Test.HUnit

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
    actual = foldl1 (&&) $ map isPrime [2,3,5,7,11,13,17,19]

test1b = TestCase $ assertEqual "Problem 31" expected actual
  where
    expected = False
    actual = foldl1 (||) $ map isPrime [(-2),(-1),0,1,4,6,8,9]

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
            , test9 ]

    runTestTT tests 
