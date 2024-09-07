module SUTD.Compiler.MathExpLexerWithRegexSpec  where 

import Test.Hspec
import Prelude hiding (lex)
import SUTD.Compiler.MathExpToken
import SUTD.Compiler.MathExpLexerWithRegex


spec :: Spec
spec = do 
    describe "SUTD.Compiler.MathExpLexerWithRegex" $ do
        it "test lexOne +" $ 
            let result = lexOne "+"
                expected = Right (PlusTok, "")
            in result `shouldBe` expected 
        it "test lex 1+2*3" $
            let result = lex "1+2*3" 
                expected = Right [IntTok 1,PlusTok,IntTok 2,AsterixTok,IntTok 3]
            in result `shouldBe` expected
