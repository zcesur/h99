module ADT.TreeSpec where

import Test.Hspec
import ADT.Tree

tree = Node 'F' (Node 'B' (leaf 'A')
                          (Node 'D' (leaf 'C')
                                    (leaf 'E')))
                (Node 'G' Empty
                          (Node 'I' (leaf 'H')
                                    Empty)) 

spec :: Spec
spec = do
  describe "Tree" $ do
    it "can do post-order traversal" $ do
      preorder tree `shouldBe` "FBADCEGIH"
      preorder Empty `shouldBe` ""

    it "can do in-order traversal" $ do
      inorder tree `shouldBe` "ABCDEFGHI"
      inorder Empty `shouldBe` ""

    it "can do post-order traversal" $ do
      postorder tree `shouldBe` "ACEDBHIGF"
      postorder Empty `shouldBe` ""

    it "can do level-order traversal" $ do
      levelorder tree `shouldBe` "FBGADICEH"
      levelorder Empty `shouldBe` ""
