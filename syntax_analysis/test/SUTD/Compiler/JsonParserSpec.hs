module SUTD.Compiler.JsonParserSpec  where 

import Test.Hspec
import Prelude hiding (lex)
import Data.Map
import SUTD.Compiler.JsonToken
import SUTD.Compiler.JsonParser
import SUTD.Compiler.BacktrackParsec 


spec :: Spec
spec = do 
  describe "SUTD.Compiler.JSONParser" $ do
    it "test parseJList []" $ 
        let toks = [LBracket, RBracket]
            result = run parseJList (PEnv toks) 
            expected = Ok (JsonList [], PEnv [])
        in result `shouldBe` expected

    it "test parseJList [1,2]" $ 
        let toks = [LBracket, IntTok 1, Comma, IntTok 2, RBracket]
            result = run parseJList (PEnv toks) 
            expected = Ok (JsonList [IntLit 1, IntLit 2], PEnv [])
        in result `shouldBe` expected

    it "test parseJSON {'k1':1,'k2':[]}" $
        let toks = [LBrace,SQuote,StrTok "k1",SQuote,Colon,IntTok 1,Comma,SQuote,StrTok "k2",SQuote,Colon,LBracket, RBracket,RBrace]
            result = run parseJSON (PEnv toks) 
            expected = Ok (JsonObject (Data.Map.fromList [( "k1", IntLit 1), ("k2", JsonList [])]), PEnv [])
        in result `shouldBe` expected
