module Ex2Spec where 

import Test.Hspec
import Ex2  

spec :: Spec
spec = do 
    describe "Ex2Spec" $ do 
        it "test ex2: fmap (+1) (PP 1.0 1) == (PP 1.0 2)" $
            let result = fmap (+1) (PP 1.0 1) 
                expected = PP 1.0 2
            in result `shouldBe` expected
