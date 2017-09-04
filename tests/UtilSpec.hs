module UtilSpec where

import Test.Hspec
import Util

spec :: Spec
spec = do
  it "can determine if a list consists of unique elements" $ do
    allDistinct [1,2,3] `shouldBe` True
    allDistinct [1] `shouldBe` True
    allDistinct [1,1] `shouldBe` False
    allDistinct [1,2,1] `shouldBe` False

  it "can determine whether a positive integer is a power of 2" $ do
    let powersOf2 = map (2^) [0..]

    all powerOf2 (take 10 powersOf2) `shouldBe` True
    all powerOf2 [3,5,6,7,9,10,11,12] `shouldBe` False
