module Ex3Spec where 

import Test.Hspec
import Ex3 ( insert, BST(..) ) 

spec :: Spec
spec = do 

  describe "Ex3Spec" $ do 
    it "test ex3: insert 3 Empty == Node 3 Empty Empty" $
        let input    = Empty 
            result   = insert 3 input 
            expected = Node 3 Empty Empty 
        in result `shouldBe` expected

    it "test ex3: insert False (insert True (insert True input)) == Node True (Node False Empty Empty) Empty" $
        let input    = Empty 
            result   = insert False (insert True (insert True input)) 
            expected = Node True (Node False Empty Empty) Empty
        in result `shouldBe` expected

    
