module Problems54Athru60Spec where

import Test.Hspec
import Problems54Athru60
import ADT.Tree
import Util (allDistinct)

spec :: Spec
spec = do
  it "can generate all possible completely balanced binary trees" $ do
    let tss = map cbalTreeDirect [0, 1, 2, 3, 4, 10, 30]

    map length tss `shouldBe` [1, 1, 2, 1, 4, 56, 16] 
    all allDistinct tss `shouldBe` True
    all (all balanced) tss `shouldBe` True

  it "can determine whether two trees are mirroring each other" $ do
    let t11 = Node Nil 'x' (leaf 'x')
        t12 = Node (leaf 'x') 'x' Nil
        t21 = Node (Node Nil 'x' (leaf 'x'))
                   'x'
                   (Node (leaf 'x') 'x' (leaf 'x'))
        t22 = Node (Node (leaf 'x') 'x' (leaf 'x'))
                   'x'
                   (Node (leaf 'x') 'x' Nil)
        t31 = Node (Node Nil 'x' (leaf 'x')) 'x' Nil
        t32 = Node (Node (leaf 'x') 'x' Nil) 'x' Nil

    mirror t11 t12 `shouldBe` True
    mirror t21 t22 `shouldBe` True
    mirror t31 t32 `shouldBe` False

  it "can determine whether a tree is symmetric" $ do
    symmetric (Node (leaf 'x') 'x' (leaf 'x')) `shouldBe` True
    symmetric (Node (leaf 'x') 'x' Nil) `shouldBe` False

  it "can construct a BST" $ do
    let t = Node (Node (leaf 1) 2 Nil) 3 (Node Nil 5 (leaf 7))

    construct [3, 2, 5, 7, 1] `shouldBe` t
    symmetric (construct [5, 3, 18, 1, 4, 12, 21]) `shouldBe` True
    symmetric (construct [3, 2, 5, 7, 1]) `shouldBe` True

  it "can construct all symmetric and balanced binary trees" $ do
    length (symCbalTrees 5) `shouldBe` 2

  it "can construct all height balanced binary trees with the given max\
     \height" $ do
    all (<= 5) (map height (hbalTree 'x' 3)) `shouldBe` True
    all balanced (hbalTree 'x' 3) `shouldBe` True
