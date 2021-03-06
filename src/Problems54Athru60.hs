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

-- Problem 56
-- 
-- (**) Symmetric binary trees
-- 
-- Let us call a binary tree symmetric if you can draw a vertical line
-- through the root node and then the right subtree is the mirror image of
-- the left subtree. Write a predicate symmetric/1 to check whether a given
-- binary tree is symmetric. Hint: Write a predicate mirror/2 first to check
-- whether one tree is the mirror image of another. We are only interested in
-- the structure, not in the contents of the nodes.

symmetric :: Tree a -> Bool
symmetric Nil = True
symmetric (Node l x r) = mirror l r

mirror :: Tree a -> Tree a -> Bool
mirror Nil Nil = True
mirror Nil _ = False
mirror _ Nil = False
mirror t1 t2 = mirror (left t1) (right t2) && mirror (right t1) (left t2)

-- Problem 57
-- 
-- (**) Binary search trees (dictionaries)
-- 
-- Use the predicate add/3, developed in chapter 4 of the course, to write a
-- predicate to construct a binary search tree from a list of integer numbers.

construct :: Ord a => [a] -> Tree a
construct = foldl add Nil

add :: Ord a => Tree a -> a -> Tree a
add Nil x = leaf x
add (Node l v r) x
    | x == v = Node l v r
    | x < v  = Node (add l x) v r
    | x > v  = Node l v (add r x)

-- Problem 58
-- 
-- (**) Generate-and-test paradigm
-- 
-- Apply the generate-and-test paradigm to construct all symmetric,
-- completely balanced binary trees with a given number of nodes.

symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTreeDirect

-- Problem 59
-- 
-- (**) Construct height-balanced binary trees
-- 
-- In a height-balanced binary tree, the following property holds for every
-- node: The height of its left subtree and the height of its right subtree
-- are almost equal, which means their difference is not greater than one.
-- 
-- Construct a list of all height-balanced binary trees with the given
-- element and the given maximum height.

hbalTree :: a -> Int -> [Tree a]
hbalTree x h = concatMap (hbalTree' x) [-1 .. h]
  where
    hbalTree' _ (-1) = [Nil]
    hbalTree' x 0 = [leaf x]
    hbalTree' x h = [Node l x r | (hl, hr) <- [(h-1, h-2), (h-1, h-1), (h-2, h-1)]
                                , l <- hbalTree' x hl, r <- hbalTree' x hr]
