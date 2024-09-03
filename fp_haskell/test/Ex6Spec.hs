module Ex6Spec where 


import Test.Hspec
import Ex6 (rotate)

spec :: Spec
spec = do
  describe "Ex6Spec" $ do 
    it "test ex6" $
        let input = [[1,2,3], [4,5,6], [7,8,9]]
            result = rotate input  
            expected = [[7,4,1], [8,5,2], [9,6,3]] 
        in result `shouldBe` expected
