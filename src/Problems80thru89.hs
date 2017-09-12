module Problems80thru89 where

import           Data.List (sort, nub, nubBy)

import qualified Data.Graph.Inductive as Graph
import           Data.Graph.Inductive (Gr, (&), match)

-- Problem 80
-- 
-- (***) Conversions
-- 
-- Write predicates to convert between the different graph representations.
-- With these predicates, all representations are equivalent; i.e. for the
-- following problems you can always pick freely the most convenient form.
-- The reason this problem is rated (***) is not because it's particularly
-- difficult, but because it's a lot of work to deal with all the special
-- cases.

data Graph' a = Graph' [a] [(a, a)] deriving (Eq, Show)
data Adjacency a = Adjacency [(a, [a])] deriving (Eq, Show)
data Friendly a = Friendly [(a, a)] deriving (Eq, Show)

graphToAdj :: Eq a => Graph' a -> Adjacency a
graphToAdj (Graph' xs ys) = Adjacency (go xs ys)
  where
    go [] ys = []
    go (x:xs) ys = (x, ys >>= (adjacent x)) : go xs ys

graphToFri :: Eq a => Graph' a -> Friendly a
graphToFri (Graph' xs ys) = Friendly (go xs ys)
  where
    go xs ys = ys ++ map (\x -> (x, x)) (filter (isolated ys) xs)

adjToGraph :: (Eq a, Ord a) => Adjacency a -> Graph' a
adjToGraph (Adjacency xs) = Graph' (nodes xs) (edges xs)
  where
    nodes = sort . map fst
    edges = sort 
          . nubBy edgesEqual
          . concatMap (\(x, x') -> zipWith (,) (repeat x) x')

adjToFri :: Eq a => Adjacency a -> Friendly a
adjToFri (Adjacency xs) = Friendly (go xs)
  where
    go [] = []
    go (x:xs) = case snd x of
        [] -> (fst x, fst x) : go xs
        _  -> zipWith (,) (repeat (fst x)) (snd x) ++ go xs

friToGraph :: (Eq a, Ord a) => Friendly a -> Graph' a
friToGraph (Friendly xs) = Graph' (nodes xs) (edges xs)
  where
    nodes = sort . nub . concatMap (\(x, y) -> [x, y])
    edges = sort . nubBy edgesEqual . filter (\(x, y) -> x /= y)

friToAdj :: (Eq a, Ord a) => Friendly a -> Adjacency a
friToAdj = graphToAdj . friToGraph

-- | Given two edges, determine whether they are equal (assuming undirected
-- graph).
edgesEqual :: Eq a => (a, a) -> (a, a) -> Bool
edgesEqual (a,b) (c,d) = a == c && b == d ||
                         a == d && b == c

-- | Given a node and an edge, return the adjacent node.
adjacent :: Eq a => a -> (a, a) -> [a]
adjacent x (a, b)
    | x == a    = [b]
    | x == b    = [a]
    | otherwise = []

-- | Given some edges and a node, determine whether the node is isolated.
isolated :: Eq a => [(a, a)] -> a -> Bool
isolated ys x = null $ ys >>= adjacent x

-- | Problem 81
-- 
-- (**) Path from one node to another one
-- 
-- Write a function that, given two nodes a and b in a graph, returns all the
-- acyclic paths from a to b.
paths :: Int -> Int -> [(Int, Int)] -> [[Int]]
paths start end edges = go start end $ fromList edges
  where
    go :: Graph.Node -> Graph.Node -> Gr n e -> [[Graph.Node]]
    go start end g
        | Graph.isEmpty g = []
        | start == end    = [[end]]
    go start end g = case match start g of
        (Just c,  g) -> [start:path | s <- Graph.suc' c, path <- go s end g]
        (Nothing, _) -> []

-- | Construct an inductive graph with unlabeled nodes and edges with the
-- given list of edges.
fromList :: [(Int, Int)] -> Gr () ()
fromList es = Graph.mkGraph vs es'
  where
    es' = zipWith (\(x,y) z -> (x,y,z)) es (repeat ())
    vs  = zip (nub $ sort $ foldl (\acc (x,y) -> x:y:acc) [] es) (repeat ())
