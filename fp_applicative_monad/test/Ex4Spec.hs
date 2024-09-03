module Ex4Spec where 

import Test.Hspec
import Ex4  

spec :: Spec
spec = do 
  describe "Ex4Spec" $ do 

    it "test ex4 Functor: runMk (fmap (+1) (Mk (\\ s -> Nothing)) ()  == Nothing" $ 
        let input = fmap (+1) (Mk (\s -> Nothing))
            result = runMk input ()
            expected = Nothing
        in result `shouldBe` expected

    it "test ex4 Functor: runMk (fmap (+1) (Mk (\\ s -> Just (0,s))) ()  == Just (1,())" $
        let input = fmap (+1) (Mk (\s -> Just (0,s)))
            result = runMk input ()
            expected = Just (1,())
        in result `shouldBe` expected

    it "test ex4 Applicative: runMk (pure 1) () == Just (1, ())" $
        let input = pure 1
            result = runMk input () 
            expected = Just (1, ())
        in result `shouldBe` expected 

    it "test ex4 Applicative: runMk (pure (+1) <*> pure 1) () == Just (2, ())" $
        let input = pure (+1) <*> pure 1
            result = runMk input () 
            expected = Just (2, ())
        in result `shouldBe` expected 
    
    it "test ex4 Applicative: runMk (Mk (\\s -> Nothing) <*> pure 1) () == Nothing" $
        let input = Mk (\s -> Nothing) <*> pure 1
            result = runMk input () 
            expected = (Nothing :: Maybe (Int, ()))
        in result `shouldBe` expected 

    it "test ex4 Applicative: runMk (pure (+1) <*> Mk (\\s -> Nothing)) () == Nothing" $
        let input = pure (+1) <*> Mk (\s -> Nothing)
            result = runMk input () 
            expected = (Nothing :: Maybe (Int, ()))
        in result `shouldBe` expected 

    it "test ex4 Monad: runMk (pure 1 >>= \\x -> pure (x + 1)) () == Just (2,())" $
        let input = pure 1 >>= \x -> pure (x + 1)
            result = runMk input () 
            expected = Just (2,())
        in result `shouldBe` expected 


    it "test ex4 Monad: runMk (Mk (\\s -> Nothing) >>= \\x -> pure (x + 1)) () == Nothing" $
        let input = Mk (\s -> Nothing) >>= \x -> pure (x + 1)
            result = runMk input () 
            expected = (Nothing :: Maybe (Int, ()))
        in result `shouldBe` expected 
