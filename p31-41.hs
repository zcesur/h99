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

main = do
    let tests = TestList
            [ test1a 
            , test1b ]

    runTestTT tests 
