insertAt :: a -> [a] -> Int -> [a]
insertAt x ys n = concat [fst chunks, [x], snd chunks]
  where
    chunks = splitAt (n-1) ys

main = print $ insertAt 'X' "abcd" 2
