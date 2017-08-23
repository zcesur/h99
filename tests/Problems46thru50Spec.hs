module Problems46thru50Spec where

import Test.Hspec
import Problems46thru50

spec :: Spec
spec = do
    it "can generate a predicate table for a given logical expression in 2\
       \variables" $ do
      let expected = unlines ["True True True"
                             ,"True False True"
                             ,"False True False"
                             ,"False False False"]
      
      tablePure (\a b -> (and' a (or' a b))) `shouldBe` expected

    it "can generate a predicate table for a given logical expression in 2\
       \variables and predicates as operators" $ do
      let expected = unlines ["True True True"
                             ,"True False True"
                             ,"False True False"
                             ,"False False False"]
      
      tablePure (\a b -> a `and'` (a `or'` not' b)) `shouldBe` expected
    
    it "can generate a predicate table for a given logical expression in 2\
       \variables" $ do
      let actual = tablePure' 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)

      let expected = unlines ["True True True True"
                             ,"True True False True"
                             ,"True False True True"
                             ,"True False False False"
                             ,"False True True False"
                             ,"False True False False"
                             ,"False False True False"
                             ,"False False False False"]

      actual `shouldBe` expected

    it "can generate n-bit Gray code" $ do
      gray 3 `shouldBe` ["000","001","011","010","110","111","101","100"]
