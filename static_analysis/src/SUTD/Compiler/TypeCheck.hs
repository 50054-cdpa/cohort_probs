module SUTD.Compiler.TypeCheck where 

import Data.Map 
import SUTD.Compiler.Util 
import SUTD.Compiler.SimplyTypedLambdaCalculus
import Control.Monad.Except

-- | the type environment 
type Gamma = Map Var Type 


-- |  it defers from the notes, by treating Gamma |- t : T as a function instead of a relation.
typeCheck :: Gamma -> Term -> StateResult StateInfo Type
{-
 Gamma |- t1 : T1 -> T2    Gamma |- t2 : T1
--------------------------------------------- (lctApp)
 Gamma |- t1 t2 : T2
-}
typeCheck gamma (AppTerm t1 t2) = do 
    funTy <- typeCheck gamma t1 
    ty3   <- typeCheck gamma t2 
    case funTy of 
        FunTy ty1 ty2 | ty1 == ty3 -> return ty2
                      | otherwise  -> throwError ("Type error. " ++ show ty1 ++ " is not matching with " ++ show ty3)
        _ -> throwError ("ype error. " ++ show t1 ++ " is not a function")
{-
 c \in {true, false}
---------------------- (lctBool)
 Gamma |- c : bool
-}
typeCheck gamma (ConstTerm (BoolConst _)) = return BoolTy
{-
 c is an integer
---------------------- (lctInt)
 Gamma |- c : int
-}
typeCheck gamma (ConstTerm (IntConst _)) = return IntTy
{-
 Gamma |- t : (T1 -> T2) -> T1 -> T2 
--------------------------------- (lctFix)
 Gamma |- fix t : T1 -> T2
-}
typeCheck gamma (FixTerm t) = undefined -- fixme 
{-
 Gamma |- t1: bool   Gamma |- t2 : T  Gamma |- t3 : T 
------------------------------------------------------- (lctIf)
 Gamma |- if t1 then t2 else t3 : T
-}
typeCheck gamma (IfTerm t1 t2 t3) = do 
    ty1 <- typeCheck gamma t1 
    ty2 <- typeCheck gamma t2 
    ty3 <- typeCheck gamma t3
    case ty1 of 
        BoolTy | ty2 == ty3 -> return ty2
               | otherwise  -> throwError ("Type error. " ++ show (IfTerm t1 t2 t3) ++ "'s then and else branches' types are not the same.")
        _                   -> throwError ("Type error. " ++ show (IfTerm t1 t2 t3) ++ "'s condition is not in boolean type.")
{-
 The following differs from the note.
 When x already in Gamma, i.e. it is another x in the nested scope.
 we will rename x to a new name.

 Gamma oplus (x, T) |- t: T' 
------------------------------  (lctLam)
 Gamma |- \x : T. t : T -> T'
-}
typeCheck gamma (LambdaTerm x ty body) 
    | x `member` gamma = do 
        n <- newName 
        let z = Var n
            s = (x, VarTerm z) -- [z/x] 
        body' <- appSubst s body  -- alpha renaming 
        typeCheck gamma (LambdaTerm z ty body')
    | otherwise = do 
        let gamma' = insert x ty gamma
        ty' <- typeCheck gamma' body
        return (FunTy ty ty')
{-
 The following differs from the note.
 When x already in Gamma, i.e. it is another x in the nested scope.
 we will rename x to a new name.

Gamma |- t1: T1    Gamma oplus (x, T1) |- t2: T2 
------------------------------------- (lctLet)
 Gamma |- let x : T1 = t1 in t2 : T2
-}
typeCheck gamma (LetTerm x ty t1 t2) = undefined -- fixme
{-
 Gamma |- t1: int  Gamma |- t2 : int  
------------------------------------------------------------- (lctOp2)
 Gamma |- t1 == t2 : bool 

 Gamma |- t1: int  Gamma |- t2 : int  
------------------------------------------------------------- (lctOp3)
 Gamma |- t1 == t2 : bool 
-}
typeCheck gamma (OpTerm t1 DEqual t2) = do 
    ty1 <- typeCheck gamma t1 
    ty2 <- typeCheck gamma t2
    if ty1 == ty2 
    then return BoolTy
    else throwError ("Type error. " ++ show (OpTerm t1 DEqual t2) ++ " is having different operand types.")
{-
 Gamma |- t1: int  Gamma |- t2 : int  op \in {+, -, *, / }
------------------------------------------------------------- (lctOp1)
 Gamma |- t1 op t2 : int 
-}
typeCheck gamma (OpTerm t1 op t2) = do 
    ty1 <- typeCheck gamma t1
    ty2 <- typeCheck gamma t2
    if ty1 == ty2 && ty1 == IntTy
    then return IntTy
    else throwError ("Type error. " ++ show (OpTerm t1 op t2) ++ " is having non-integer operand types.")
{-
 (x, T) \in Gamma
----------------------------- (lctVar)
 Gamma |- x : T
-}
typeCheck gamma (VarTerm x) = case Data.Map.lookup x gamma of 
    Nothing -> throwError ("Type error. " ++ show x ++ " is not defined in " ++ show gamma ++ ".")
    Just ty -> return ty