import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  
  describe "someFunc" $ do
    it "returns \"someFunc\"" $
      someFunc `shouldBe` "someFunc"
  
  describe "otherFunc" $ do
    it "returns \"otherFunc\"" $
      otherFunc `shouldBe` "otherFunc"

  describe "anotherFunc" $ do
    it "returns \"anotherFunc\"" $
      anotherFunc `shouldBe` "anotherFunc"