module Problems80thru89Spec where

import Problems80thru89
import Test.Hspec

spec :: Spec
spec = do
  describe "Solution 80" $ do
    it "can convert between various graph representations" $ do
      let nodes = ['b','c','d','f','g','h','k']
          edges = [('b','c'),('b','f'),('c','f'),('f','k'),('g','h')]
          graph = Graph nodes edges
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
