module Ex5Spec where 
import Test.Hspec
import Ex5
import Control.Monad.Reader 
import Control.Monad.Identity

spec :: Spec
spec = do 
  describe "Ex5Spec" $ do 

    it "test ex5 rmPrint" $ 
        let input :: BTree Int 
            input = Node 5 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) Empty
            result = runReaderT (rmPrint input) (Env 0)
            expected = Identity "5\n\
\    3\n\
\        1\n\
\            <empty>\n\
\            <empty>\n\
\        4\n\
\            <empty>\n\
\            <empty>\n\
\    <empty>"
        in result `shouldBe` expected