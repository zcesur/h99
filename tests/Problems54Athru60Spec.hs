module Problems54Athru60Spec where

import Test.Hspec
import Problems54Athru60
import ADT.Tree (balanced)
import Util (allDistinct)

spec :: Spec
spec = do
  it "can generate all possible completely balanced binary trees" $ do
    let tss = map cbalTreeDirect [0, 1, 2, 3, 4, 10, 30]

    map length tss `shouldBe` [1, 1, 2, 1, 4, 56, 16] 
    all allDistinct tss `shouldBe` True
    all (all balanced) tss `shouldBe` True
