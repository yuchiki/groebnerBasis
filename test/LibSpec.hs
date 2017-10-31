module LibSpec (spec) where

import           Lib
import           Test.Hspec

spec :: Spec
spec =
  describe "Lib.helloWorld" $ do
    context "when empty was given" $ do
      it "should return a world." $ do
        helloWorld "" `shouldBe` " World!"
      it "should not return a road." $ do
        helloWorld "" `shouldNotBe` " Road!"
    context "when polynomial was given" $ do
      it "should return the polynomial world." $ do
        helloWorld "polynomial" `shouldBe` "polynomial World!"
      
