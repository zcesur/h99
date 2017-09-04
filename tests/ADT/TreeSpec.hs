module ADT.TreeSpec where

import Test.Hspec
import ADT.Tree

-- Replicate the tree at https://en.wikipedia.org/wiki/Tree_traversal

tree = f where
    c = leaf 'C'
    e = leaf 'E'
    d = Node c 'D' e
    a = leaf 'A'
    b = Node a 'B' d
    h = leaf 'H'
    i = Node h 'I' Nil
    g = Node Nil 'G' i
    f = Node b 'F' g

spec :: Spec
spec = do
  describe "Tree" $ do
    it "can do post-order traversal" $ do
      preorder tree `shouldBe` "FBADCEGIH"
      preorder Nil `shouldBe` ""

    it "can do in-order traversal" $ do
      inorder tree `shouldBe` "ABCDEFGHI"
      inorder Nil `shouldBe` ""

    it "can do post-order traversal" $ do
      postorder tree `shouldBe` "ACEDBHIGF"
      postorder Nil `shouldBe` ""

    it "can do level-order traversal" $ do
      levelorder tree `shouldBe` "FBGADICEH"
      levelorder Nil `shouldBe` ""

    it "can determine the height" $ do
      height tree `shouldBe` 3
      height (Node (Node Nil 'x' Nil) 'x' Nil) `shouldBe` 1
      height (Node Nil 'x' Nil) `shouldBe` 0
      height Nil `shouldBe` (-1)

    it "can determine whether a tree is balanced" $ do
      balanced tree `shouldBe` False
      balanced (left tree) `shouldBe` True
      balanced (right tree) `shouldBe` False
