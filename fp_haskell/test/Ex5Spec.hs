module Ex5Spec where 


import Test.Hspec
import Ex5 (mergesort)

spec :: Spec
spec = do
  describe "Ex5Spec" $ do 
    it "test ex5: mergesort [3,17,8,9,11,200,0,5] == [0, 3, 5, 8, 9, 11, 17, 200]" $
        let result = mergesort [3,17,8,9,11,200,0,5] 
            expected = [0, 3, 5, 8, 9, 11, 17, 200]
        in result `shouldBe` expected
