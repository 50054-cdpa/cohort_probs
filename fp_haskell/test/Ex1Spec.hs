module Ex1Spec where 

import Test.Hspec
import Ex1 (fib)

spec :: Spec
spec = do
  describe "Ex1Spec" $ do 
    it "test ex1: fib 1 == 1" $
        let result = fib 1 
            expected = 1
        in result `shouldBe` expected
    it "test ex1: fib 5 == 8" $
        let result = fib 5
            expected = 8
        in result `shouldBe` expected        
    it "test ex1: fib 10 == 89" $
        let result = fib 10
            expected = 89
        in result `shouldBe` expected

