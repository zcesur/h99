import System.Random

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

main = diff_select 6 49 >>= print
