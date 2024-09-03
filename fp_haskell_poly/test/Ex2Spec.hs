module Ex2Spec where 

import Test.Hspec
import Ex2 ( quicksort ) 

spec :: Spec
spec = do 
  describe "Ex2Spec" $ do 
    it "test ex2: quicksort [3,17,8,9,11,200,0,5] == [0, 3, 5, 8, 9, 11, 17, 200]" $
        let result = quicksort [3,17,8,9,11,200,0,5] 
            expected = [0, 3, 5, 8, 9, 11, 17, 200]
        in result `shouldBe` expected
