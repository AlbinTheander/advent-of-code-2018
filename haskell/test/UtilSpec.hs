module UtilSpec (main, spec) where

import Test.Hspec
import Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Util" $
  describe "parseInts" $ do
    it "can parse a string with only a number" $
      parseInts "12" `shouldBe` [12]
    it "can parse a string with some leading chars and a number" $
      parseInts "a12" `shouldBe` [12]
    it "can parse a more complicated example" $
      parseInts "#12 @ 43x995: 34x39" `shouldBe` [12, 43, 995, 34, 39]
