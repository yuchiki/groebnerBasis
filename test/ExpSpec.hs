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
        context "when negative input" $
            it "should parse" $
                parshow "-1" `shouldBe` "-1"
        context "when power of constant value input" $
            it "should parse" $
                parshow "10^2" `shouldBe` "100"
    describe "show" $ do
        context "when 'na' is given" $ do
            it "should show 1" $
                parshow "1" `shouldBe` "1"
            it "should show 0" $
                parshow "0" `shouldBe` "0"
            it "should not show 1 in 1a" $
                parshow "1a" `shouldBe` "a"
            it "should show 2 in 2a" $
                parshow "2a" `shouldBe` "2a"
            it "should show a+b^2 in correct order" $
                parshow "a+b^2" `shouldBe` "b^2+a"
        context "when negative term is given" $ do
            it "should show -a" $
                parshow "-a" `shouldBe` "-a"
            it "should omit '+' before '-' " $
                parshow "a-b" `shouldBe` "a-b"


parshow :: String -> String
parshow = either show show . Exp.parse
