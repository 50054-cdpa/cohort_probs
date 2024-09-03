module SUTD.Compiler.MathExpParserSpec  where 

import Test.Hspec
import Prelude hiding (lex)
import SUTD.Compiler.MathExpToken
import SUTD.Compiler.MathExpParser
import SUTD.Compiler.BacktrackParsec 

spec :: Spec
spec = do 
  describe "SUTD.Compiler.MathExpParserSpec" $ do
    it "test parse 1+2*3" $
      let toks = [IntTok 1, PlusTok, IntTok 2, AsterixTok, IntTok 3]
          result = run parseExp (PEnv toks) 
          expected = Ok (PlusExp (FactorTerm (Factor 1)) (TermExp (MultTerm (FactorTerm (Factor 2)) (Factor 3))), PEnv [])
      in result `shouldBe` expected
