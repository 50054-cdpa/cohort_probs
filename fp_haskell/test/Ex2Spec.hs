module Ex2Spec where 

import Test.Hspec
import Ex2 (len)

spec :: Spec
spec = do
  describe "Ex2Spec" $ do 
    it "test ex2: len [] == 0" $
        let result = len [] 
            expected = 0
        in result `shouldBe` expected
    it "test ex2: len [1,2,3] == 3" $
        let result = len [1,2,3]
            expected = 3
        in result `shouldBe` expected        
    it "test ex2: len ['a'..'z'] == 26" $
        let result = len ['a'..'z']
            expected = 26
        in result `shouldBe` expected

