package sutd.compiler

import sutd.compiler.SimplyTypedLambdaCalculus.*
import sutd.compiler.Monad.*
import sutd.compiler.StateT.*
import sutd.compiler.Util.*

object TypeCheck { 
    import Term.* 
    import Type.* 
    import Value.* 
    import Const.*
    import Op.* 

    /**
      * the type environments
      */
    type Gamma = Map[Var,Type]

    /**
      * it defers from the notes, by treating Gamma |- t : T as a function instead of a relation.
      * 
      *
      * @param g - the type environment
      * @param t - the lambda term
      * @return type of t under g
      */
    def typeCheck(g:Gamma, t:Term)(using m:StateResultMonadError[StateInfo]):StateResult[Type] = t match {
        /** Gamma |- t1 : T1 -> T2    Gamma |- t2 : T1
          * --------------------------------------------- (lctApp)
          *  Gamma |- t1 t2 : T2
          */
        case AppTerm(t1, t2) => for {
            funTy <- typeCheck(g,t1) 
            ty3 <- typeCheck(g, t2) 
            ty <- funTy match {
                case FunTy(ty1, ty2) if ty1 == ty3 => m.pure(ty2)
                case FunTy(ty1, ty2) => m.raiseError(s"Type error. ${ty1} is not matching with ${ty3}")
                case _ => m.raiseError(s"Type error. ${t} is not a function")
            }
        } yield ty
        /**
          * c \in {true, false}
          * ---------------------- (lctBool)
          * Gamma |- c : bool
          *
          * c is an integer
          * ---------------------- (lctInt)
          * Gamma |- c : int
          */
        case ConstTerm(c) => c match {
            case BoolConst(v) => m.pure(BoolTy)
            case IntConst(v) => m.pure(IntTy)
        } 
        /** 
          *  Gamma |- t : (T1 -> T2) -> T1 -> T2 
          * --------------------------------- (lctFix)
          *  Gamma |- fix t : T1 -> T2
          */
        // TODO
        case FixTerm(t) => m.raiseError("TODO: fixme")
        /**
          *  Gamma |- t1: bool   Gamma |- t2 : T  Gamma |- t3 : T 
          * ------------------------------------------------------- (lctIf)
          *  Gamma |- if t1 then t2 else t3 : T
          */
        case IfTerm(t1, t2, t3) => for {
            ty1 <- typeCheck(g, t1)
            ty2 <- typeCheck(g, t2)
            ty3 <- typeCheck(g, t3)
            ty <- ty1 match {
                case BoolTy if (ty2 == ty3) => m.pure(ty2)
                case BoolTy => m.raiseError(s"Type error. ${t}'s then and else branches' types are not the same.")
                case _ => m.raiseError(s"Type error. ${t}'s condition is not in boolean type.")
            }
        } yield ty

        /**
          *  The following differs from the note.
          *  When x is already in Gamma, i.e. it is another x in the nested scope.
          *  we will rename x to a new name.
          * 
          *  Gamma oplus (x, T) |- t: T' 
          * ------------------------------  (lctLam)
          *  Gamma |- \x : T. t : T -> T'
          */
        case LambdaTerm(x, ty, body) if g.contains(x) => for {
            n   <- newName
            z   <- m.pure(Var(n))
            s   <- m.pure((x,VarTerm(z))) // [z/x]
            bodyp <- appSubst(s, body) // alpha renaming
            ty <- typeCheck(g, LambdaTerm(z, ty, bodyp))
        } yield ty
        case LambdaTerm(x, ty, body) => for { 
            typ <- typeCheck(g + (x -> ty), body)
        } yield FunTy(ty, typ)
        /**
          *  The following differs from the note.
          *  When x is already in Gamma, i.e. it is another x in the nested scope.
          *  we will rename x to a new name.
          * 
          * Gamma |- t1: T1    Gamma oplus (x, T1) |- t2: T2 
          * ------------------------------------- (lctLet)
          *  Gamma |- let x : T1 = t1 in t2 : T2
          */
        // TODO
        case LetTerm(x, ty, t1, t2) if g.contains(x) => m.raiseError("TODO fixme")
        // TODO
        case LetTerm(x, ty, t1, t2) => m.raiseError("TODO fixme")
        /**
          *  Gamma |- t1: int  Gamma |- t2 : int  
          * ------------------------------------------------------------- (lctOp2)
          * Gamma |- t1 == t2 : bool 
          */
        /**
          *  Gamma |- t1: bool  Gamma |- t2 : bool  
          * ------------------------------------------------------------- (lctOp3)
          * Gamma |- t1 == t2 : bool 
          */
        case OpTerm(t1, DEqual, t2) => for {
            ty1 <- typeCheck(g, t1)
            ty2 <- typeCheck(g, t2) 
            ty <- if (ty1 == ty2) { 
                m.pure(BoolTy)
            } else {
                m.raiseError(s"Type error. ${t} is having different operand types.")
            }
        } yield ty
        /**
          *  Gamma |- t1: int  Gamma |- t2 : int  op \in {+, -, *, / }
          * ------------------------------------------------------------- (lctOp1)
          * Gamma |- t1 op t2 : int 
          */
        case OpTerm(t1, op, t2) => for {
            ty1 <- typeCheck(g, t1)
            ty2 <- typeCheck(g, t2) 
            ty <- if ((ty1 == ty2) && (ty1 == IntTy)) { 
                m.pure(IntTy)
            } else {
                m.raiseError(s"Type error. ${t} is having non-integer operand types.")
            }
        } yield ty

        /**
          * (x, T) \in Gamma
          * ----------------------------- (lctVar)
          * Gamma |- x : T
          */
        case VarTerm(x) => g.get(x) match {
            case None => m.raiseError(s"Type error. ${x} is not defined in ${g}.")
            case Some(ty) => m.pure(ty)
        }
    }
}