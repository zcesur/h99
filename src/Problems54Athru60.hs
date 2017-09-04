module Problems54Athru60 where

import ADT.Tree
import Control.Monad (replicateM)
import Problems21thru28 (combinations)
import Util (powerOf2, flg)

-- Problem 55
-- 
-- (**) Construct completely balanced binary trees
-- 
-- In a completely balanced binary tree, the following property holds for
-- every node: The number of nodes in its left subtree and the number of
-- nodes in its right subtree are almost equal, which means their difference
-- is not greater than one.
-- 
-- Write a function cbal-tree to construct completely balanced binary trees
-- for a given number of nodes. The predicate should generate all solutions
-- via backtracking. Put the letter 'x' as information into all nodes of the
-- tree. 

-- | Given a number of nodes, construct all possible completely balanced binary
-- trees directly without backtracking by first constructing the largest
-- perfect tree and then inserting sets of leaf nodes.
cbalTreeDirect :: Int -> [Tree Char]
cbalTreeDirect n
    | extra == 0 = [baseTree]
    | otherwise  = map (foldl insertAt baseTree) allNodePaths
  where
    baseSize = 2 ^ flg (n + 1) - 1
    baseHeight = flg (baseSize + 1) - 1
    baseTree = mkPerfectTree baseSize
    extra = n - baseSize
    allNodePaths = combinations extra $ replicateM (baseHeight + 1) [L,R]

data Branch = L | R deriving Eq
type Path = [Branch]

-- | Given a number of nodes, make a perfect tree, if possible.
mkPerfectTree :: Int -> Tree Char
mkPerfectTree n
    | n == 0                 = Nil
    | not $ powerOf2 (n + 1) = error "Not enough nodes."
    | otherwise              = foldl insertAt Nil nodePaths
  where
    height = flg (n + 1) - 1
    nodePaths = concatMap (flip replicateM [L,R]) [0 .. height]

-- | Given a tree and a valid path, insert a new leaf node at the path.
insertAt :: Tree Char -> Path -> Tree Char
insertAt Nil [] = leaf 'x'
insertAt Nil (x:xs) = error "One or more ancestors are missing."
insertAt (Node l v r) [] = error "The path collides with an existing node."
insertAt (Node l v r) (x:xs) = case x of
    L -> Node (insertAt l xs) v r
    R -> Node l v (insertAt r xs)
