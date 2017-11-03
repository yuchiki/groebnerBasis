module ExpSpec (spec) where

import Exp
import Test.Hspec

spec :: Spec
spec = do
    describe "parse" $ do
        context "when expression has const" $
            it "should parse" $
                parshow "(2+1)^2" `shouldBe` "9"
        context "when nested expression" $ do
            it "should flatten expression" $
                parshow "a(b+c)" `shouldBe` "ab+ac"
            it "should deal coefficient correctly" $
                parshow "(a+b)^2" `shouldBe` "a^2+2ab+b^2"
    describe "show" $
        context "when 'na' is given" $ do
            it "should not show 1 in 1a" $
                parshow "1a" `shouldBe` "a"
            it "should show 2 in 2a" $
                parshow "2a" `shouldBe` "2a"


parshow :: String -> String
parshow = either show show . Exp.parse
