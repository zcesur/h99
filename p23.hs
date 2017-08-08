import System.Random

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    let indices = take n (randomRs (0, (length xs) - 1) gen)
    return $ map (xs !!) indices

main = rnd_select "abcdefgh" 3 >>= putStrLn
