module Problems90thru94 where

import Data.List (permutations, nub)

-- | Problem 90
-- 
-- (**) Eight queens problem
-- 
-- This is a classical problem in computer science. The objective is to place
-- eight queens on a chessboard so that no two queens are attacking each
-- other; i.e., no two queens are in the same row, the same column, or on the
-- same diagonal.
queens n = filter (\x -> valid $ zip x [1..]) $ permutations [1..n]
  where
    -- Given a list of coordinates, determine whether there are diagonally
    -- attacking queens.
    valid xs = not $ dups (map (uncurry (+)) xs) ||
                     dups (map (uncurry (-)) xs)
    dups xs = nub xs /= xs
