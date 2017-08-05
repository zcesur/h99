removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (z, ys ++ zs)
  where
    ys = take (n-1) xs
    (z:zs) = drop (n-1) xs

main = print $ removeAt 2 "abcd"
