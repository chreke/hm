module Main where

import Test.Hspec
import Parser

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "should parse variable" $ do
      compile "x" `shouldBe` Right (Var "x")
      
    it "should parse function" $ do
      compile "fun x -> x" `shouldBe` Right (Fun "x" (Var "x"))
      
    it "should parse application" $ do
      compile "f x" `shouldBe` Right (App (Var "f") (Var "x"))
      
    it "should parse nested application" $ do
      compile "f x y" `shouldBe` Right (App (App (Var "f") (Var "x")) (Var "y"))
      
    it "should parse complex expression" $ do
      compile "fun y -> y fun x -> x 10" `shouldBe` 
        Right (Fun "y" (App (Var "y") (Fun "x" (App (Var "x") (Var "10")))))
      
    it "should fail on invalid input" $ do
      case compile "fun ->" of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure "Expected parser to fail"