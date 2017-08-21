module ADT.Tree
( Tree(..)
, leaf
, preorder
, inorder
, postorder
, levelorder
) where

data Tree a = Empty | Node a (Tree a) (Tree a)
              deriving (Show, Eq)

isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

getVal :: Tree a -> a
getVal Empty = undefined
getVal (Node x _ _) = x

children :: Tree a -> [Tree a]
children Empty = []
children (Node _ l r) = filter (not . isEmpty) [l,r]

leaf :: a -> Tree a
leaf x = Node x Empty Empty

preorder :: Tree a -> [a]
preorder Empty = []
preorder (Node x left right) = x : (preorder left) ++ (preorder right)

inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

postorder :: Tree a -> [a]
postorder Empty = []
postorder (Node x left right) = postorder left ++ postorder right ++ [x]

levelorder :: Tree a -> [a]
levelorder Empty = []
levelorder x = lo [x]
  where
    lo [] = []
    lo xs = map getVal xs ++ lo (concatMap children xs)
