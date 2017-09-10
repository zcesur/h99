module Problems90thru94Spec where

import Problems90thru94
import Test.Hspec

spec :: Spec
spec = do
  describe "queens" $ do
    it "can generate all lists of non-attacking eight queens boards." $ do
      length (queens 4) `shouldBe` 2
      length (queens 8) `shouldBe` 92
