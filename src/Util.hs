module Util where

-- | Determine whether all elements of a list are unique.
allDistinct :: Eq a => [a] -> Bool
allDistinct [] = True
allDistinct (x:xs) = all (x/=) xs && allDistinct xs

-- | Determine whether a positive integer is a power of 2.
powerOf2 :: Int -> Bool
powerOf2 1 = True
powerOf2 n = (n `mod` 2 == 0) && (powerOf2 (n `div` 2) || n `div` 2 == 0)

-- | Compute the floor of base-2 log of a positive integer.
flg :: Int -> Int
flg = floor . logBase 2 . fromIntegral
