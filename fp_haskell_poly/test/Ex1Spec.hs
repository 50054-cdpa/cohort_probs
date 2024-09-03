module Ex1Spec where 

import Test.Hspec
import Ex1 ( Fraction(Fraction) ) 

spec :: Spec
spec = do
    describe "Ex1Spec" $ do 
        it "test ex1: Fraction 1 2 == Fraction 1 2" $
            let result = Fraction 1 2 == Fraction 1 2
                expected = True
            in result `shouldBe` expected

        it "test ex1: Fraction 1 2 == Fraction 3 6" $
            let result = Fraction 1 2 == Fraction 3 6
                expected = True 
            in result `shouldBe` expected

        it "test ex1: not (Fraction 1 2 == Fraction 3 4)" $
            let result = Fraction 1 2 == Fraction 3 4
                expected = False
            in result `shouldBe` expected

        it "test ex1: compare (Fraction 1 2) (Fraction 1 2) == EQ" $
            let result = Fraction 1 2 `compare` Fraction 1 2
                expected = EQ
            in result `shouldBe` expected

        it "test ex1: compare (Fraction 1 2) (Fraction 3 6) == EQ" $
            let result = Fraction 1 2 `compare` Fraction 3 6
                expected = EQ 
            in result `shouldBe` expected

        it "test ex1: compare (Fraction 1 2) (Fraction 3 4) == LT" $
            let result = Fraction 1 2 `compare` Fraction 3 4
                expected = LT
            in result `shouldBe` expected

        it "test ex1: compare (Fraction 5 7) (Fraction 1 4) == GT" $
            let result = Fraction 5 7 `compare` Fraction 1 4
                expected = GT
            in result `shouldBe` expected
