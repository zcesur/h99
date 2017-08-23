module Problems01thru10Spec where

import Test.Hspec
import Problems01thru10

spec :: Spec
spec = do
    it "can find the last element" $ do
      last' [1,2,3,4] `shouldBe` 4

    it "can find the last but one element" $ do
      penultimate ['a'..'z'] `shouldBe` 'y'

    it "can find the k-th element" $ do
      elementAt "haskell" 5 `shouldBe` 'e'

    it "can find the length" $ do
      length' "Hello, world!" `shouldBe` 13

    it "can reverse" $ do
      reverse' [1,2,3,4] `shouldBe` [4,3,2,1]

    it "can find out if it's a palindrome" $ do
      isPalindrome "madamimadam" `shouldBe` True

    it "can flatten" $ do
      let xs = (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
      flatten xs `shouldBe` [1,2,3,4,5]

    it "can eliminate consecutive duplicates" $ do
      compress "aaaabccaadeeee" `shouldBe` "abcade"

    it "can pack consecutive duplicates into sublists" $ do
      pack "aaaabccaadeeee" `shouldBe` ["aaaa","b","cc","aa","d","eeee"]
      
    it "can do run-length encoding" $ do
      let xs = [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
      encode "aaaabccaadeeee" `shouldBe` xs

