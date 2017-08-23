module Problems21thru28Spec where

import Test.Hspec
import Problems21thru28

spec :: Spec
spec = do
    it "can insert an element at a given position" $ do
      insertAt 'X' "abcd" 2 `shouldBe` "aXbcd"

    it "can create a list containing all integers within a given range" $ do
      range 4 9 `shouldBe` [4,5,6,7,8,9]

    it "can generate the combinations of K distinct objects chosen from\
       \the N elements of a list" $ do
      length (combinations 3 [1..12]) `shouldBe` 220

    it "can group the elements of a set of size 9 into disjoint subsets\
       \of 2, 3 and 4 elements" $ do
      length (group3 ['a'..'i']) `shouldBe` 1260

    it "can group the elements of a set into disjoint subsets for an\
       \arbitrary list of group sizes" $ do
      length (group [2,3,4] ['a'..'i']) `shouldBe` 1260
      length (group [2,2,5] ['a'..'i']) `shouldBe` 756

    it "can sort a list of lists according to length" $ do
      let xs = ["abc","de","fgh","de","ijkl","mn","o"]
      let xs' = ["o","de","de","mn","abc","fgh","ijkl"]
      lsort xs `shouldBe` xs'

    it "can sort a list of lists according to the length frequency" $ do
      let xs = ["abc","de","fgh","de","ijkl","mn","o"]
      let xs' = ["ijkl","o","fgh","abc","mn","de","de"]
      lfsort xs `shouldBe` xs'
