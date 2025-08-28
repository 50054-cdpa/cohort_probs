package sutd.compiler

import sutd.compiler.SimplyTypedLambdaCalculus.*
import sutd.compiler.Monad.*
import sutd.compiler.StateT.*
import sutd.compiler.Util.*


object BigStepEval {
    import Term.*
    import Value.*

    /**
      * implementing big step operational semantics for lambda calculus
      *
      * @param t - a lambda calculus term
      * @param m - the StateResultMonadError dictionary with all the monad error API
      * @return - stateful result which is either an error message or a value
      */
    def eval(t:Term)(using m:StateResultMonadError[StateInfo]):StateResult[Value] = t match {
        case ConstTerm(c) => m.pure(ConstValue(c))
        case VarTerm(v)   => m.raiseError("Unbound variable.")
        case LambdaTerm(_,_,_) => m.pure(LambdaValue(t))
        case FixTerm(t1) => for {
            v1   <- eval(t1)
            t1pp <- v1 match {
                case LambdaValue(LambdaTerm(f, ty, t1p)) => appSubst((f, FixTerm(t1)), t1p)
                case _ => m.raiseError("fix is applied to a non-lambda term.")
            }
            v2 <- eval(t1pp)
        } yield v2
        case AppTerm(t1,t2) => for {
            v1  <- eval(t1) 
            t3p <- v1 match {
                case LambdaValue(LambdaTerm(x,ty, t3)) => appSubst((x, t2), t3)
                case _ => m.raiseError("the left subterm of a function application does not evaluate to a lambda value.")
            }
            v2 <- eval(t3p)
        } yield v2
        case IfTerm(t1, t2, t3) => for {
            v1 <- eval(t1) 
            v  <- if (isTrue(v1)) { eval(t2) } else { eval(t3) }
        } yield v
        case LetTerm(x, ty, t1, t2) => for {
            tp <- appSubst((x,t1),t2) 
            v  <- eval(tp) 
        } yield v 
        case OpTerm(t1, op, t2) => for {
            v1 <- eval(t1) 
            v2 <- eval(t2)
            v  <- op match {
                    case Op.DEqual => equal(v1,v2)
                    case Op.Plus   => plus(v1,v2) 
                    case Op.Minus  => minus(v1,v2)
                    case Op.Mult   => mult(v1,v2) 
                    case Op.Div    => div(v1,v2) 
                }
        } yield v
    }

    import Const.*

    /**
      * check whether the two input values v1 and v2 are the same type and equal.
      *
      * @param v1
      * @param v2
      * @param m - the StateResultMonadError dictionary with all the monad error API
      * @return either an error or a value
      */
    def equal(v1:Value, v2:Value)(using m:StateResultMonadError[StateInfo]):StateResult[Value] = (v1,v2) match {
        case (ConstValue(IntConst(u1)), ConstValue(IntConst(u2))) => m.pure(ConstValue(BoolConst(u1 == u2)))
        case (ConstValue(BoolConst(u1)), ConstValue(BoolConst(u2))) => m.pure(ConstValue(BoolConst(u1 == u2)))
        case (_,_) => m.raiseError("type mismatch for equality test.")
    }

    /**
      * compute the sum of two input values v1 and v2 if they are of type int.
      *
      * @param v1
      * @param v2
      * @param m - the StateResultMonadError dictionary with all the monad error API
      * @return either an error or a value
      */
    def plus(v1:Value, v2:Value)(using m:StateResultMonadError[StateInfo]):StateResult[Value] = (v1,v2) match {
        case (ConstValue(IntConst(u1)), ConstValue(IntConst(u2))) => m.pure(ConstValue(IntConst(u1 + u2)))
        case (_,_) => m.raiseError("type mismatch for plus operation.")
    }


    /**
      * compute the difference of two input values v1 and v2 if they are of type int.
      *
      * @param v1
      * @param v2
      * @param m - the StateResultMonadError dictionary with all the monad error API
      * @return either an error or a value
      */
    def minus(v1:Value, v2:Value)(using m:StateResultMonadError[StateInfo]):StateResult[Value]  = (v1,v2) match {
        case (ConstValue(IntConst(u1)), ConstValue(IntConst(u2))) => m.pure(ConstValue(IntConst(u1 - u2)))
        case (_,_) => m.raiseError("type mismatch for minus operation.")
    }

    /**
      * compute the product of two input values v1 and v2 if they are of type int.
      *
      * @param v1
      * @param v2
      * @param m - the StateResultMonadError dictionary with all the monad error API
      * @return either an error or a value
      */
    def mult(v1:Value, v2:Value)(using m:StateResultMonadError[StateInfo]):StateResult[Value]  = (v1,v2) match {
        case (ConstValue(IntConst(u1)), ConstValue(IntConst(u2))) => m.pure(ConstValue(IntConst(u1 * u2)))
        case (_,_) => m.raiseError("type mismatch for mult operation.")
    }

    /**
      * compute the quotient of two input values v1 and v2 if they are of type int.
      *
      * @param v1
      * @param v2
      * @param m - the StateResultMonadError dictionary with all the monad error API
      * @return either an error or a value
      */
    def div(v1:Value, v2:Value)(using m:StateResultMonadError[StateInfo]):StateResult[Value]  = (v1,v2) match {
        case (ConstValue(IntConst(u1)), ConstValue(IntConst(u2))) if u2 != 0 => m.pure(ConstValue(IntConst(u1 / u2)))
        case (ConstValue(IntConst(u1)), ConstValue(IntConst(u2)))            => m.raiseError("div by zero error.")
        case (_,_) => m.raiseError("type mismatch for div operation.")
    }

    /**
      * isTrue returns true if the input value is a boolean value and it is true.
      *
      * @param v - a value, either a constant or a lambda abstraction
      * @return boolean
      */
    def isTrue(v:Value):Boolean = v match {
        case ConstValue(c) => c match {
            case BoolConst(v) => v
            case IntConst(v) => false
        }
        case LambdaValue(l) => false
    }


}