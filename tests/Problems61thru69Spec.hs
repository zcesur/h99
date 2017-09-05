module Problems61thru69Spec where

import Problems61thru69
import ADT.Tree
import Test.Hspec

tree :: Tree Int
tree = Node (Node Nil 2 (leaf 4)) 1 (leaf 2)

spec :: Spec
spec = do
  describe "countLeaves" $ do
    it "can count leaves of a binary tree" $ do
     countLeaves tree `shouldBe` 2 

  describe "leaves" $ do
    it "can collect the leaves of a binary tree in a list" $ do
      leaves tree `shouldBe` [4,2]

  describe "internals" $ do
    it "can collect the internal nodes of a binary tree in a list" $ do
      internals tree `shouldBe` [1,2]

  describe "atLevel" $ do
    it "can collect the nodes at a given level in a list" $ do
      atLevel tree 2 `shouldBe` [2,2]
