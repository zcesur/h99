module Problems80thru89Spec where

import           Problems80thru89
import           Test.Hspec

import           Data.List (sort)
import qualified Data.Graph.Inductive as Graph
import           Data.Graph.Inductive (Gr, Node, Edge, LEdge)


spec :: Spec
spec = do
  describe "Solution 80" $ do
    it "can convert between various graph representations" $ do
      let nodes = ['b','c','d','f','g','h','k']
          edges = [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
          graph = Graph' nodes edges
          adj = Adjacency [ ('b', "cf")
                          , ('c', "bf")
                          , ('d', "")
                          , ('f', "bck")
                          , ('g', "h")
                          , ('h', "g")
                          , ('k', "f")
                          ]

      graphToAdj graph `shouldBe` adj
      adjToGraph adj `shouldBe` graph

      friToGraph (graphToFri graph) `shouldBe` graph
      friToAdj (adjToFri adj) `shouldBe` adj

  describe "Solution 81" $ do
    it "can return all acyclic paths from a to b" $ do
      let g = [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
      paths 1 4 g `shouldBe` [[1,2,3,4],[1,3,4]]
      paths 2 6 g `shouldBe` []
  
  describe "Solution 82" $ do
    it "can return all cycles from a given node" $ do
      let g  = [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
          g' = g ++ [(3,1),(4,1)]

      cycles 2 g `shouldBe` [[2,3,4,2]]
      cycles 1 g `shouldBe` []
      cycles 1 g' `shouldBe` [[1,2,3,1],[1,2,3,4,1],[1,3,1],[1,3,4,1]]

  describe "Solution 84" $ do
    it "can construct the MST" $ do
      let edges = [(1,2,12),(1,3,34),(1,5,78),(2,4,55)
                  ,(2,5,32),(3,4,61),(3,5,44),(4,5,93)]
          nodes = zip [1,2,3,4,5] ['A'..]
          g = Graph.mkGraph nodes edges
      sort (prim g) `shouldBe` [(1,2,12),(1,3,34),(2,4,55),(2,5,32)]
