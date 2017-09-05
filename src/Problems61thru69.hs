module Problems61thru69 where

import ADT.Tree

-- Problem 61
-- 
-- Count the leaves of a binary tree
-- 
-- A leaf is a node with no successors. Write a predicate count_leaves/2 to
-- count them.

countLeaves :: Tree a -> Int
countLeaves Nil = 0
countLeaves (Node Nil _ Nil) = 1
countLeaves (Node l x r) = countLeaves l + countLeaves r

-- Problem 61A
-- 
-- Collect the leaves of a binary tree in a list
-- 
-- A leaf is a node with no successors. Write a predicate leaves/2 to collect
-- them in a list.

leaves :: Tree a -> [a]
leaves Nil = []
leaves (Node Nil x Nil) = [x]
leaves (Node l x r) = leaves l ++ leaves r

-- Problem 62
-- 
-- Collect the internal nodes of a binary tree in a list
-- 
-- An internal node of a binary tree has either one or two non-empty
-- successors. Write a predicate internals/2 to collect them in a list.

internals :: Tree a -> [a]
internals Nil = []
internals (Node Nil x Nil) = []
internals (Node l x r) = [x] ++ internals l ++ internals r

-- Problem 62B
-- 
-- Collect the nodes at a given level in a list
-- 
-- A node of a binary tree is at level N if the path from the root to the
-- node has length N-1. The root node is at level 1. Write a predicate
-- atlevel/3 to collect all nodes at a given level in a list.

atLevel :: Tree a -> Int -> [a]
atLevel Nil _ = []
atLevel (Node _ x _) 1 = [x]
atLevel (Node l _ r) n = atLevel l (n-1) ++ atLevel r (n-1)
