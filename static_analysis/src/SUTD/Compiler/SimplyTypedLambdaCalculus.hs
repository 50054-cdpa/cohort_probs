module SUTD.Compiler.SimplyTypedLambdaCalculus where 

import Prelude hiding ((-))
import Data.Set 


-- | Simply Typed Lambda Calculus Terms
data Term = ConstTerm Const -- ^ c 
    | VarTerm Var           -- ^ x
    | LambdaTerm Var Type Term   -- ^ \x:T.t 
    | AppTerm Term Term     -- ^ t t
    | IfTerm Term Term Term -- ^ if t then t else t
    | OpTerm Term Op Term   -- ^ t 
    | LetTerm Var Type Term Term -- ^ let x:T = t in t
    | FixTerm Term          -- ^ fix t
    deriving (Show, Eq)

data Type = IntTy | BoolTy | FunTy Type Type deriving (Show, Eq)

-- | Variables
data Var = Var String deriving (Show, Eq, Ord) 

-- | Constants
data Const = IntConst Int | BoolConst Bool deriving (Show, Eq)

-- | Binary Operators
data Op = Plus  -- ^ + 
    | Minus     -- ^ -
    | Mult      -- ^ *
    | Div       -- ^ /
    | DEqual    -- ^ == 
    deriving (Show, Eq)

-- | All the possible values
data Value = ConstValue Const   -- ^ c 
    | LambdaValue Term          -- ^ \x.t
    deriving (Show, Eq)

(-) :: Set Var -> Var -> Set Var 
(-) = flip delete


-- | Compute the set of free variables in term t
fv :: Term -> Set Var 
fv (VarTerm y)         = singleton y
fv (LambdaTerm x _ body) = (fv body) - x
fv (AppTerm t1 t2)     = fv t1 `union` fv t2
fv (LetTerm x _ t1 t2)   = ((fv t1) - x) `union` fv t2 
fv (IfTerm t1 t2 t3)   = fv t1 `union` fv t2 `union` fv t3
fv (ConstTerm c)       = empty 
fv (FixTerm t1)        = fv t1 
fv (OpTerm t1 op t2)   = fv t1 `union` fv t2