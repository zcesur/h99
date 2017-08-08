import System.Random

-- TO DO:
--
-- 1) Figure out why n = m case does not terminate
-- 2) Handle n > m case

diff_select :: Int -> Int -> IO [Int]
diff_select n m = do
    gen <- getStdGen
    let draws = randomRs (1, m) gen
    return $ last . takeWhile (isSmall n) . scanl addIfDistinct [] $ draws
      where isSmall n xs = length xs <= n
            addIfDistinct acc x
              | x `elem` acc = acc
              | otherwise    = x:acc

main = diff_select 6 49 >>= print
