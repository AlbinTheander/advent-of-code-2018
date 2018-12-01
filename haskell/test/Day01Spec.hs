module Day01Spec (main, spec) where

import Test.Hspec
import Day01

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Day 01" $
  describe "findDuplicateFreq" $
    it "can handle the provided examples" $ do
      findDuplicateFreq [1, -1] `shouldBe` 0
      findDuplicateFreq [3, 3, 4, -2, -4] `shouldBe` 10
      findDuplicateFreq [-6, 3, 8, 5, -6] `shouldBe` 5
      findDuplicateFreq [7, 7, -2, -7, -4] `shouldBe` 14
