module Day01Spec (main, spec) where

import Test.Hspec
import Day01

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Day 01" $
  describe "parseData" $
    it "can parse a list of signed numbers, one per line" $
      parseData "+1\n-2\n+23\n-6786" `shouldBe` [1, -2, 23, -6786]
