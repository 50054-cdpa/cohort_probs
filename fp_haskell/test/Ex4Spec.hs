module Ex4Spec where 

import Test.Hspec
import Ex4 (flatten)

spec :: Spec
spec = do
  describe "Ex4Spec" $ do 
    it "test ex4: flatten [] == []" $
        let result = flatten ([]::[[Int]]) 
            expected = []
        in result `shouldBe` expected
    it "test ex4: flatten [[1], [2], [3]] == [1,2,3]" $
        let result = flatten [[1], [2], [3]] 
            expected = [1,2,3]
        in result `shouldBe` expected        
    it "test ex4: flatten [['a'],['b'],['c'],['d'] == ['a'..'d']" $
        let result = flatten [['a'],['b'],['c'],['d']] 
            expected =  ['a'..'d']
        in result `shouldBe` expected

