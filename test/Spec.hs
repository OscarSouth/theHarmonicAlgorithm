import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do

  describe "Generate -- overtoneSets" $ do
    it "test not implemented" $
      overtoneSets 3 `shouldBe` overtoneSets 3

  describe "Generate -- zeroForms" $ do
    it "test not implemented" $
      zeroForms [[1,2,3],[4,5,6]] `shouldBe` zeroForms [[1,2,3],[4,5,6]]

  describe "Generate -- zeroForm" $ do
    it "test not implemented" $
      zeroForm [1,2,3] `shouldBe` zeroForm [1,2,3]

  describe "Generate -- zeroTrans" $ do
    it "test not implemented" $
      zeroTrans 4 2 `shouldBe` zeroTrans 4 2

  describe "Generate -- sort" $ do
    it "test not implemented" $
      sort [1,2,3] `shouldBe` sort [1,2,3]

  describe "Generate -- choose" $ do
    it "test not implemented" $
      choose 2 [0..4] `shouldBe` choose 2 [0..4]