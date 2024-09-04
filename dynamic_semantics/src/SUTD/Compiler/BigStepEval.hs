{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module SUTD.Compiler.BigStepEval where



import Control.Monad.State
import Control.Monad.Except
import SUTD.Compiler.LambdaCalculus hiding ((-))
import Data.Set (member)

type Subst = (Var, Term)

type Err = String

data StateInfo = StateInfo {nextNum::Int} deriving (Show, Eq)

data Result a = Error String
    | Ok a
    deriving (Show, Eq)



instance Functor Result where
    fmap f (Error msg) = Error msg
    fmap f (Ok a) = Ok (f a)


instance Applicative Result where
    pure a = Ok a
    (Error msg) <*> _ = Error msg
    _ <*> (Error msg) = Error msg
    (Ok f) <*> (Ok a) = Ok (f a)

instance Monad Result where
    (Error msg) >>= _ = Error msg
    (Ok a) >>= f = f a

instance MonadError Err Result where
    throwError msg = Error msg
    catchError (Error msg) handler = handler msg
    catchError (Ok a) _ = Ok a

type StateResult s = StateT s Result

{-
-- we don't need this. MonadError e (StateT s e) has been defined in Control.Monad.State
instance MonadError Err (StateResult s) where
    throwError :: Err -> StateResult s a
    throwError msg = StateT (\_ -> Error msg)
    catchError sa handler = StateT (\s -> case runStateT sa s of
        { Error msg -> runStateT (handler msg) s
        ; Ok a      -> Ok a
        })
-}

newName :: StateResult StateInfo String
newName = do
    stInfo <- get
    _      <- put stInfo{nextNum=(nextNum stInfo) + 1}
    return ("_x_$" ++ show (nextNum stInfo))

appSubst :: Subst -> Term -> StateResult StateInfo Term
appSubst s (ConstTerm c) =  undefined -- fixme
appSubst (x, u) (VarTerm y)
    | y == x    = return u
    | otherwise = return (VarTerm y)
appSubst s (AppTerm t1 t2) = do
    t3 <- appSubst s t1
    t4 <- appSubst s t2
    return (AppTerm t3 t4)
appSubst s (IfTerm t1 t2 t3) = do
    t4 <- appSubst s t1
    t5 <- appSubst s t2
    t6 <- appSubst s t3
    return (IfTerm t4 t5 t6)
appSubst (x, u) (LetTerm y t2 t3) = undefined -- fixme 
appSubst (x, u) (LambdaTerm y t2)
    | (y /= x) && y `member` (fv u) = do
        t3 <- appSubst (x, u) t2
        return (LambdaTerm y t3)
    | otherwise = do
        {- Substitution Application would fail because lambda bound variable is clashing with the substitution domain. 
           or substitution domain is captured in the body of lambda abstraction. 
           instead of failing, we apply alpha renaming to y and t2 immediately 
        -}
        n <- newName
        let z = Var n
            s2 = (y, VarTerm z) -- [z/y]
        t2' <- appSubst s2 t2   -- alpha renaming 
        t3  <- appSubst (x,u ) t2'   -- subst after alpha renaming 
        return (LambdaTerm z t3)
appSubst (x, u) (FixTerm t) = do
    t' <- appSubst (x, u) t
    return (FixTerm t')
appSubst (x, u) (OpTerm t1 op t2) = do
    t1' <- appSubst (x, u) t1
    t2' <- appSubst (x, u) t2
    return (OpTerm t1' op t2')


-- | implementing big step operational semantics for lambda calculus
eval :: Term -> StateResult StateInfo Value 
eval (ConstTerm c) = undefined -- fix me
eval (VarTerm v)   = throwError "Unbound variable."
eval (LambdaTerm x t) = return (LambdaValue (LambdaTerm x t))
eval (FixTerm t1) = do 
    v1 <- eval t1
    t1'' <- case v1 of 
        { LambdaValue (LambdaTerm f t1') -> appSubst (f, FixTerm t1) t1'
        ; _ -> throwError "fix is applied to a non-lambda term." 
        }
    eval t1'' 
eval (AppTerm t1 t2) = undefined -- fix me
eval (IfTerm t1 t2 t3) = do 
    v1 <- eval t1 
    if isTrue v1 
    then eval t2
    else eval t3
eval (LetTerm x t1 t2) = do 
    t' <- appSubst (x,t1) t2
    eval t'
eval (OpTerm t1 op t2) = do 
    v1 <- eval t1 
    v2 <- eval t2 
    case op of 
        DEqual -> equal v1 v2 
        Plus   -> plus v1 v2 
        Minus  -> minus v1 v2 
        Mult   -> mult v1 v2
        Div    -> divide v1 v2


-- | check whether the two input values v1 and v2 are the same type and equal.
equal :: Value -> Value -> StateResult StateInfo Value 
equal (ConstValue (IntConst u1)) (ConstValue (IntConst u2)) = return (ConstValue (BoolConst (u1 == u2)))
equal (ConstValue (BoolConst u1)) (ConstValue (BoolConst u2)) = return (ConstValue (BoolConst (u1 == u2)))
equal _ _ = throwError "type mismatch for equality test."

-- | compute the sum of two input values v1 and v2 if they are of type int.
plus :: Value -> Value -> StateResult StateInfo Value
plus (ConstValue (IntConst u1)) (ConstValue (IntConst u2)) = return (ConstValue (IntConst (u1 + u2)))
plus _ _ = throwError "type mismatch for plus operation."

-- | compute the difference of two input values v1 and v2 if they are of type int.
minus :: Value -> Value -> StateResult StateInfo Value
minus (ConstValue (IntConst u1)) (ConstValue (IntConst u2)) = return (ConstValue (IntConst (u1 - u2)))
minus _ _ = throwError "type mismatch for minus operation."


-- | compute the product of two input values v1 and v2 if they are of type int.
mult :: Value -> Value -> StateResult StateInfo Value
mult (ConstValue (IntConst u1)) (ConstValue (IntConst u2)) = return (ConstValue (IntConst (u1 * u2)))
mult _ _ = throwError "type mismatch for mult operation."


-- | compute the quotient of two input values v1 and v2 if they are of type int.
divide :: Value -> Value -> StateResult StateInfo Value
divide (ConstValue (IntConst u1)) (ConstValue (IntConst u2)) 
    | u2 /= 0 = return (ConstValue (IntConst (u1 `div` u2)))
    | otherwise = throwError "divide by zero error."
divide _ _ = throwError "type mismatch for mult operation."


-- | isTrue returns true if the input value is a boolean value and it is true.
isTrue :: Value -> Bool 
isTrue (ConstValue (BoolConst v)) = v
isTrue _ = False