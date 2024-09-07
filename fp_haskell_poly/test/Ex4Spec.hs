module Ex4Spec where 

import Test.Hspec
import Ex3 ( insert, BST(..) ) 
import Ex4 ( subtree ) 

spec :: Spec
spec = do 
    describe "Ex4Spec" $ do 
        it "test ex4: subtree 5 (insert 4 (insert 5 (insert 3 Empty))) == Just (Node 5 (Node 4 Empty Empty) Empty)" $
            let input    = insert 4 (insert 5 (insert 3 Empty))
                result   = subtree 5 input 
                expected = Just (Node 5 (Node 4 Empty Empty) Empty) 
            in result `shouldBe` expected

    
