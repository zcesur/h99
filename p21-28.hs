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

main = do
    let tests = TestList
            [ test1
            , test2 ]

    runTestTT tests 
