module SUTD.Compiler.TypeCheckSpec  where 

import Test.Hspec
import Data.Map (empty)
import Control.Monad.State
import SUTD.Compiler.SimplyTypedLambdaCalculus
import SUTD.Compiler.Util (StateInfo(..), Result(..))
import SUTD.Compiler.TypeCheck (typeCheck)


st :: StateInfo
st = StateInfo 0


spec :: Spec
spec = do 
  describe "SUTD.Compiler.TypeCheck" $ do
    it "const 1 should have type int" $ 
        let input = ConstTerm (IntConst 1)
            gamma = Data.Map.empty 
            result = runStateT (typeCheck gamma input) st
            expected = Ok (IntTy, StateInfo 0)
        in result `shouldBe` expected

    it "identity function should be have type int -> int" $ 
        let input = LambdaTerm (Var "x") IntTy (VarTerm (Var "x")) -- \x:int.x 
            gamma = Data.Map.empty
            result = runStateT (typeCheck gamma input) st
            expected = Ok (FunTy IntTy IntTy, StateInfo 0)
        in result `shouldBe` expected 

    it "factorial should have type int -> int" $ 
        let three = ConstTerm (IntConst 3)
            zero  = ConstTerm (IntConst 0) 
            one   = ConstTerm (IntConst 1)
            varx  = Var "x" 
            varf  = Var "f" 
            tyx = IntTy
            tyf = FunTy IntTy IntTy
            cond  = OpTerm (VarTerm varx) DEqual zero -- x == 0
            ifelse = IfTerm cond one (OpTerm (VarTerm varx) Mult (AppTerm (VarTerm varf) (OpTerm (VarTerm varx) Minus one))) -- if x == 0 then 1 else x * (f (x - 1))
            fac   = FixTerm (LambdaTerm varf tyf (LambdaTerm varx tyx ifelse)) -- fix \f. \x. if == 0 then 1 else x * (f (x - 1))
            input = fac 
            gamma = Data.Map.empty
            result = runStateT (typeCheck gamma input) st
            expected = Ok (FunTy IntTy IntTy, StateInfo 0)
        in result `shouldBe` expected 