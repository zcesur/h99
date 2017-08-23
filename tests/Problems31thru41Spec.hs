module Problems31thru41Spec where

import Test.Hspec
import Problems31thru41

spec :: Spec
spec = do
    it "can determine whether a given integer is prime" $ do
      (and . map isPrime) [2,3,5,7,11,13,17,19] `shouldBe` True
      (or . map isPrime) [(-2),(-1),0,1,4,6,8,9] `shouldBe` False

    it "can determine the GCD of two positive integers" $ do
      [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6] `shouldBe` [9,3,3]

    it "can determine whether two positive integers are coprime" $ do
      coprime 35 64 `shouldBe` True

    it "can calculate Euler's totient function" $ do
      totient 10 `shouldBe` 4

    it "can determine the prime factors of a given positive integer" $ do
      primeFactors 315 `shouldBe` [3,3,5,7]
      prime_factors_mult 315 `shouldBe` [(3,2),(5,1),(7,1)]

    it "can calculate Euler's totient function (improved)" $ do
      phi 10 `shouldBe` 4

    it "can construct a list of all prime numbers given in a range" $ do
      primesR 10 20 `shouldBe` [11,13,17,19]

    it "can find two prime numbers that sum up to a given even integer" $ do
      goldbach 28 `shouldBe` (5,23)

    it "can find a Goldbach composition of all even integers in a range" $ do
      goldbachList 9 20 `shouldBe` [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
