module Ex3Spec where 

import Test.Hspec
import Ex3 (lift)

spec :: Spec
spec = do
  describe "Ex3Spec" $ do 
    it "test ex3: lift [] == []" $
        let result = lift ([]::[Int]) 
            expected = []
        in result `shouldBe` expected
    it "test ex3: lift [1,2,3] == [[1], [2], [3]]" $
        let result = lift [1,2,3]
            expected = [[1], [2], [3]]
        in result `shouldBe` expected        
    it "test ex3: lift ['a'..'d'] == [['a'],['b'],['c'],['d']" $
        let result = lift ['a'..'d']
            expected = [['a'],['b'],['c'],['d']]
        in result `shouldBe` expected

