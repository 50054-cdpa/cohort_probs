module SUTD.Compiler.JsonLexerWithRegexSpec  where 

import Test.Hspec
import Prelude hiding (lex)
import SUTD.Compiler.JsonToken
import SUTD.Compiler.JsonLexerWithRegex


spec :: Spec
spec = do 
  describe "SUTD.Compiler.JSONLexerWithRegex" $ do
    it "test lexOne ," $ 
      let result = lexOne ","
          expected = Right (Comma, "")
      in result `shouldBe` expected 
    it "test lex {'k1':1,'k2':[]}" $
      let result = lex "{'k1':1,'k2':[]}" 
          expected = Right [LBrace,SQuote,StrTok "k1" ,SQuote,Colon, IntTok 1,Comma,SQuote,StrTok "k2",SQuote,Colon,LBracket, RBracket,RBrace]
      in result `shouldBe` expected
