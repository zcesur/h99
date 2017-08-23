module Problems11thru20Spec where

import Test.Hspec
import Problems11thru20

spec :: Spec
spec = do
    it "can do run-length encoding" $ do
      let xs = [Multiple 4 'a'
               ,Single 'b'
               ,Multiple 2 'c'
               ,Multiple 2 'a'
               ,Single 'd'
               ,Multiple 4 'e']

      encodeModified "aaaabccaadeeee" `shouldBe` xs

    it "can decode a run-length encoded list" $ do
      let xs = [Multiple 4 'a'
               ,Single 'b'
               ,Multiple 2 'c'
               ,Multiple 2 'a'
               ,Single 'd'
               ,Multiple 4 'e']

      decodeModified xs `shouldBe` "aaaabccaadeeee"

    it "can do run-length encoding directly" $ do
      let xs = [Multiple 4 'a'
               ,Single 'b'
               ,Multiple 2 'c'
               ,Multiple 2 'a'
               ,Single 'd'
               ,Multiple 4 'e']

      encodeDirect "aaaabccaadeeee" `shouldBe` xs

    it "can duplicate" $ do
      dupli [1,2,3] `shouldBe` [1,1,2,2,3,3]

    it "can replicate" $ do
      repli "abc" 3 `shouldBe` "aaabbbccc"

    it "can drop every Nth element" $ do
      dropEvery "abcdefghik" 3 `shouldBe` "abdeghk"

    it "can split a list into two parts" $ do
      split "abcdefghik" 3 `shouldBe` ("abc", "defghik")

    it "can extract a slice" $ do
      slice ['a','b','c','d','e','f','g','h','i','k'] 3 7 `shouldBe` "cdefg"

    it "can rotate" $ do
      rotate ['a','b','c','d','e','f','g','h'] 3 `shouldBe` "defghabc"
      rotate ['a','b','c','d','e','f','g','h'] (-2) `shouldBe` "ghabcdef"

    it "can remove Kth element" $ do
      removeAt 2 "abcd" `shouldBe` ('b',"acd")
